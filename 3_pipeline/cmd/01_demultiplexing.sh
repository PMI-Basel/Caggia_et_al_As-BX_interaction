#!/bin/bash
#SBATCH --time=06:00:00
#SBATCH --mem=10g
#SBATCH --output=run.out
#SBATCH --error=run.error
#SBATCH --job-name=demultiplexing
#SBATCH --cpus-per-task=41
#SBATCH --mail-user=jan.waelchli@unibas.ch
#SBATCH --mail-type=ALL

#load modules
module load foss/2018b #interpreters
module load FastQC/0.11.8-Java-1.8
module load cutadapt/3.4-foss-2018b-Python-3.6.6
module load xlsx2csv/0.7.4-foss-2018b-Python-3.6.6
module load pigz/2.4-GCCcore-7.3.0

## --------------------------------------------------------------------
## Jan Wälchli | 15.02.2022 | Version 1.0
## --------------------------------------------------------------------

#running time notification
echo 'Start script'

#convert the design file from xlsx to tab
xlsx2csv ../../1_start/design.xlsx design.csv
cat design.csv | tr ',' '\t' | tail -n +2 > design.tab
rm design.csv

#get the runs
 runs=$(awk '{print $3}' design.tab | sort | uniq)

## --------------------------------------------------------------------
## A | Quality Control - FastQC
## --------------------------------------------------------------------

#create output folder
rm -r  ../../4_output/qc 2> /dev/null
mkdir -p ../../4_output/qc

#quality control
srun fastqc -t 20 -k 0 -q ../../2_data/* -o ../../4_output/qc 2> /dev/null
wait #wait until srun jobs are finished

#remove no longer needed files
rm ../../4_output/qc/*.zip

#running time notification
echo 'A - Quality Control done'

# --------------------------------------------------------------------
# B | Primer Files
# --------------------------------------------------------------------

#create folder
rm -r primer_cutted 2> /dev/null
mkdir -p primer_cutted/primers/

#create files

#bacteria
for run in ${runs}; do
  #forward
	 grep ${run} design.tab |\
	 awk '{print $6, $7}' | sort | uniq |\
   sed 's/^/>/' | sed 's/ /\n/g' \
	 > primer_cutted/primers/${run}_forward_primers.fasta
   #reverse
   grep ${run} design.tab |\
   awk '{print $8, $9}' | sort | uniq |\
   sed 's/^/>/' | sed 's/ /\n/g' \
   > primer_cutted/primers/${run}_reverse_primers.fasta
done

#running time notification
echo 'B - Primer Files done'

# # ---------------------------------------------------------------------
# # C | Primer cutting
# # ---------------------------------------------------------------------

#loop over all runs
for run in ${runs}; do
  #output folder
  mkdir primer_cutted/${run}/
  #cut primers (parallized in srun jobs)
  srun cutadapt -g file:primer_cutted/primers/${run}_forward_primers.fasta \
                -o primer_cutted/${run}/${run}_r1_cutted.fastq.gz -j 20 \
                ../../2_data/${run}_r1.fastq.gz \
                > primer_cutted/${run}/${run}_summary_forward_primer_cutting.txt
  srun cutadapt -g file:primer_cutted/primers/${run}_reverse_primers.fasta \
                -o primer_cutted/${run}/${run}_r2_cutted.fastq.gz \
                ../../2_data/${run}_r2.fastq.gz -j 20 \
                > primer_cutted/${run}/${run}_summary_reverse_primer_cutting.txt
done

wait #wait until srun jobs are finished

#running time notification
echo 'C - Primer cutting done'

# --------------------------------------------------------------------
# D | Barcode Files
# --------------------------------------------------------------------

#create folder
rm -r  demultiplexed 2> /dev/null
mkdir -p demultiplexed/barcodes

#create files
for run in ${runs}; do
  #forward
   grep ${run} design.tab |\
   awk '{print $4, $5}' | sort | uniq |\
   sed 's/^/>/' | sed 's/ /\n/g' \
   > demultiplexed/barcodes/${run}_barcodes.fasta
done

#running time notification
echo 'D - Barcode Files done'

# ---------------------------------------------------------------------
# E | Demultiplexing
# ---------------------------------------------------------------------

for run in ${runs}; do
  #decompress runs
  srun gunzip -c primer_cutted/${run}/${run}_r1_cutted.fastq.gz > temp_r1.fastq
  srun gunzip -c primer_cutted/${run}/${run}_r2_cutted.fastq.gz > temp_r2.fastq
  wait #wait until srun jobs are finished
  #create output folder
	mkdir demultiplexed/${run}

  #read barcode-name and barcode-sequence
  while read name;
   do read seq;
   #create output name (remove '>')
   outname=$(echo ${name} | tr -d '>')
   ###forward reads
   #grep reads with the barcode in the header & save in a new file
   grep -A3 -e ^@\.\*${seq} --no-group-separator temp_r1.fastq > demultiplexed/${run}/${run}_${outname}_F.fastq
   ###reverse reads
   grep -A3 -e ^@\.\*${seq} --no-group-separator temp_r2.fastq > demultiplexed/${run}/${run}_${outname}_R.fastq
   echo ${outname} "demultiplexed"
 done < demultiplexed/barcodes/${run}_barcodes.fasta

 #gzip files
 pigz -p 40 demultiplexed/${run}/*

 #remove temporary files
 rm temp_r1.fastq temp_r2.fastq
done

#running time notification
echo 'E - Demultiplexing done'

# ---------------------------------------------------------------------
# F | Clean up
# ---------------------------------------------------------------------

#sort files by taxa

for run in ${runs}; do

	taxa=$(grep ${run} design.tab | awk '{print $2}' | sort | uniq)

	#bacteria
	 if [[ ${taxa} == *'b'* ]]; then
		mkdir -p demultiplexed/bacteria/${run} 2> /dev/null #may already exist
		bac_primers=$(grep ${run} design.tab | awk '{if ($2 == "b") print $0;}' | awk '{print $4}' | sort | uniq)
		for b in ${bac_primers}; do
			mv demultiplexed/${run}/*${b}* demultiplexed/bacteria/${run}
		done
	fi

	#fungi
	 if [[ ${taxa} == *'f'* ]]; then
		mkdir -p demultiplexed/fungi/${run} 2> /dev/null #may already exist
		fun_primers=$(grep ${run} design.tab | awk '{if ($2 == "f") print $0;}' | awk '{print $4}' | sort | uniq)
		for f in ${fun_primers}; do
			mv demultiplexed/${run}/*${f}* demultiplexed/fungi/${run}
		done
	fi

  rmdir demultiplexed/${run} #delete folder if empty

done

#running time notification
echo 'F - Clean up done'

# ---------------------------------------------------------------------
# G | sequences tracking
# ---------------------------------------------------------------------

rm ../../2_data/seqs.txt 2> /dev/null
touch ../../2_data/seqs.txt

#number of raw sequences per run
for run in ${runs}; do

	#raw
	raw_lines=$(gunzip -c ../../2_data/${run}_r1.fastq.gz | wc -l)
	raw_seqs=$(echo ${raw_lines} / 4 | bc) #each seq has 4 lines

	for species in bacteria fungi; do

		#input
		path_in=demultiplexed/${species}/${run}/

		#count number of demultiplexed sequences
		if [ -d "${path_in}" ]; then #check if path exist, otherwise set to NA
			dem_lines=$(gunzip -c ${path_in}*F.fastq* | wc -l)
			dem_seqs=$(echo ${dem_lines} / 4 | bc) #each seq has 4 lines
		else dem_seqs="NA"
		fi

		#save to corresponding taxa
		if [ ${species} == "bacteria" ]; then
			dem_seqs_bac=$(echo ${dem_seqs})
		else dem_seqs_fun=$(echo ${dem_seqs})
		fi

	done

	#output
	echo ${run} "raw" ${raw_seqs} "bac" ${dem_seqs_bac} "fun" ${dem_seqs_fun} >> ../../2_data/seqs.txt

done

echo 'E - sequences tracking done'
echo 'End script'
