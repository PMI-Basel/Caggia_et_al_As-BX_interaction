
union_three <- function(a,b,c) 
{union(union(a,b),c)}
union_four<- function(a,b,c,d) 
{union(union(union(a,b),c),d)}
union_five<- function(a,b,c,d,e) 
{union(union(union(union(a,b),c),d),e)}

intersect_three <- function(a,b,c) 
{intersect(intersect(a,b),c)}
intersect_four <- function(a,b,c,d) 
{intersect(intersect(intersect(a,b),c),d)}
intersect_five <- function(a,b,c,d,e) 
{intersect(intersect(intersect(intersect(a,b),c),d),e)}
