library(tidyverse)
library(data.table)
library(stringr)

df=read.csv("/Users/jordanthompson/Desktop/Important stuff/Programing Stuff/Senators by generation/US senate.csv")


 while(length(ind <- which(df$Senator == "")) > 0){
   df$Senator[ind] <- df$Senator[ind -1]
}

while(length(ind <- which(df$Lifespan == "")) > 0){
  df$Lifespan[ind] <- df$Lifespan[ind -1]
}

df=df%>%
  mutate(year_born=substring(Lifespan,1,4))%>%
  transmute(year_born,In_office=Years,Senator)%>%
  filter(year_born>1880)%>%
  mutate(year_born=as.integer(year_born))


names=c("Lost Generation (1880-1900)","Greatest Generation (1901-1925)", "Silent Generation (1926-1945)","Baby Boomers (1946-1964)","Gen X (1965-1980)", "Millennial (1981-1996)","GenZ (1997-2012)")
years=c("1880-1900","1901-1925","1926-1945","1946-1964",'1965-1980',"1981-1996","1997-2012")
df_generations=data_frame(names,years)


#df for each year
#filter for born after 1880
#assign generation



df_generations=df_generations %>%
  mutate(start=as.numeric(substring(years,1,4)))%>%
  mutate(end=as.numeric(substring(years,6,9)))

dt_generations=as.data.table(df_generations)
dt_generations=dt_generations[, .(year_born = start:end), by = .(names)][]

df_generations=as.data.frame(dt_generations)

df=left_join(df,df_generations,by="year_born")

df=df%>%
  mutate(start=as.integer(substring(In_office,1,4)), end=as.integer(substring(In_office,6,9)))


#graph line plot, x=year,y=percent_gerenation color=generation position = stacked geom_area

df1=df%>%
  filter(is.na(df$end))%>%
  transmute(Senator,names,In_office)

df2=df%>%
  filter(!is.na(df$end))%>%
  transmute(Senator,names,start,end)


df2=df2%>%
  mutate(In_office = map2(start, end, seq)) %>%
  unnest(In_office) %>%
  ## deal with overlaps by keeping the max start age:
  group_by(In_office,Senator) %>%
  slice_max(start) %>%
  ungroup()%>%
  transmute(Senator,names,In_office)

df1$In_office=as.integer(df1$In_office)
df=bind_rows(df1,df2)

df3=read.csv("/Users/jordanthompson/Downloads/list1.csv")
View(df3)
df3=df3%>%
  transmute(Assumed.office,Senator,Born)%>%
  mutate(year_born=as.numeric(substring(df3$Born,2,5)))%>%
  mutate(In_office=str_sub(df3$Assumed.office,start=-4))%>%
  transmute(Senator,In_office,year_born)
df3=left_join(df3,df_generations,by="year_born")

df3=df3%>%
  mutate(year_born=NULL)%>%
  mutate(start=In_office,end=2023)%>%
  
  
  
  
  mutate(In_office = map2(start, end, seq)) %>%
  unnest(In_office) %>%
  ## deal with overlaps by keeping the max start age:
  group_by(In_office,Senator) %>%
  slice_max(start) %>%
  ungroup()%>%
  transmute(Senator,names,In_office)
df=bind_rows(df,df3)

df=df%>%
  transmute(year=In_office,gen_names=names)%>%
  group_by(year)%>%
  count(gen_names)%>%
  mutate(Number_of_Senators=n,Generation=gen_names)

df$Generation=factor(df$Generation, levels = c("Lost Generation (1880-1900)","Greatest Generation (1901-1925)", "Silent Generation (1926-1945)","Baby Boomers (1946-1964)","Gen X (1965-1980)", "Millennial (1981-1996)","GenZ (1997-2012)"))

  


ggplot(df,aes(x=year,y=Number_of_Senators,color=Generation))+
 geom_line()+
  labs(x="Year", y="Number of Senators", title= "U.S Senator Generation Cohort by Year")+
  theme_classic()