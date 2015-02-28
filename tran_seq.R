#交易序列模式判断
pattern_detect<-function(x,i,j,k)
{
  if(which.min(x)==1 & which.max(x)== length(x)  )
  {
    print("Growth Pattern")
  }
  
  if(which.min(x)==length(x) & which.max(x)== 1)
  {
    print("Deciline Pattern")
  }
  
    
  if( any( ( (x[which.max(x)]-x) / x[which.max(x)]) < i  ) )
  {
    print("Top Pattern")
  }
  
  if( any( ( (x-x[which.min(x)]) / x[which.min(x)] ) > i ) )
  {
    print("Bottom Pattern")
  }
}