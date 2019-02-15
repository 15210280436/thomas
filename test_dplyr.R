library(tidyverse)
library(nycflights13)
library(ggplot2)
library(DBI)
library(modelr) #
library(charlatan)
library(stringr)  

con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                      host   = "127.0.0.1",
                      dbname = "octopus_susuan",
                      user      = "wangjf",
                      password      = "d4aSXN2P",
                      port     = 5000)

student <- tbl(con,"o_student")

student_1 <- head(student,n=100) %>% 
  collect()

student_1

dbDisconnect(con)

mpg
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),color = "orange") +
  facet_wrap(~ class,nrow = 3)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),color = "orange") +
  facet_grid(drv ~ cyl)

mpg %>% 
  distinct(cyl)

flights %>% 
  filter(month==1 & day==1,is.na(dep_time)) #is.na()选择有缺失值的

flights %>%
  arrange(desc(is.na(dep_time)))

df <- tibble(x = c(5, 2, NA)) 
arrange(df, x)

flights %>% 
  rename(year_1=year) 

flights %>% 
  mutate(strptime(dep_time,'%H:%M:%S %Y')) #表示两个字段之间所有列

flights %>% 
  transmute(dep_time,hour=dep_time %/% 60,minit=dep_time %%60) #转变时间

flights %>% 
  select(-(year:dep_delay)) #表示不在两个字段之间所有列

flights_sml <- flights %>% 
  select(year:day,ends_with("delay"),distance,air_time) #ends_with结尾包含delay的lie
flights_sml %>% 
  mutate(gain=air_time-dep_delay,
         speed=distance/air_time*60)
flights_sml %>% 
  transmute(gain=air_time-dep_delay, #transmute只保留增加的变量
            speed=distance/air_time*60)

flights %>% 
  select(origin,tailnum,dep_time) %>% 
  group_by(origin,tailnum) %>% 
  arrange(desc(dep_time)) 

flights %>% 
  group_by(year,month,day) %>% 
  summarise(delay=mean(dep_delay,na.rm = TRUE)) #na.rm 去掉空值

student_1 %>% 
  filter(grade<8) %>% 
  ggplot(mapping=aes(x=grade,color=grade))+
  geom_bar()

# geom_point and geom_smooth 配合使用，可以看到散点以及趋势

student_1 %>% 
  filter(grade<8) %>% 
  ggplot(mapping=aes(x=grade,y=province_id))+
  geom_boxplot()

seq(1,10)

by_dest <- flights %>% 
  group_by(dest)
delay <- by_dest %>% 
  summarise(count=n(),dist=mean(distance,na.rm=TRUE),delay=mean(arr_delay,na.rm=TRUE))
delay <- filter(delay,count>20,dest!="HNL")

delay %>% 
  filter(dist<750) %>% 
  ggplot(mapping = aes(x=dist,y=delay))+
  geom_point(aes(size=count),alpha=1/3)+
  geom_smooth(se=FALSE)

flights %>% 
  group_by(tailnum) %>% 
  summarise(delay=mean(arr_delay,na.rm=TRUE),n=n()) %>% 
  ggplot(mapping=aes(x=n,y=delay))+
  geom_point(alpha=1/10)

diamonds %>% 
  filter(carat<3) %>% 
  ggplot(mapping = aes(x=carat,color=cut))+
  geom_freqpoly(binwidth=0.1)

diamonds %>% #直方图+密度图
  filter(carat<3) %>% 
  ggplot(mapping = aes(x=carat,y=..density..))+
  geom_histogram(binwidth=0.1)+
  geom_density(alpha=.7)

flights %>% 
  select(origin,tailnum,dep_time) %>% 
  group_by(origin,tailnum) %>% 
  arrange(desc(dep_time)) 

flights %>% 
  group_by(year,month,day) %>% 
  summarise(delay=mean(dep_delay,na.rm = TRUE)) #na.rm 去掉空值

diamonds1 <- diamonds %>% 
  mutate(y_1=ifelse(y<3 | y>20,NA,y))

diamonds1 %>% 
  ggplot(mapping = aes(x=x,y=y_1))+
  geom_point()

#看一下取消航班和未取消航班延误时间的样本量
flights %>% 
  mutate(
    cancelled=is.na(dep_time),# is.na =true表示dep_time为空的取消航班标签。
    sched_hour=sched_dep_time %/% 100,
    sched_min=sched_dep_time %% 100,
    sched_dep_time=sched_hour+sched_min/60
  ) %>% 
  group_by(cancelled) %>% 
  summarise(n=n())

#通过图形查看取消航班和未取消航班延误时间的差异，未取消航班远远多于取消航班数量，不能说明两者存在差异
flights %>% 
  mutate(
    cancelled=is.na(dep_time),
    sched_hour=sched_dep_time %/% 100,
    sched_min=sched_dep_time %% 100,
    sched_dep_time=sched_hour+sched_min/60
  ) %>% 
  ggplot(mapping = aes(x=sched_dep_time,color=cancelled))+
  geom_freqpoly(binwidth=1/4)

#通过密度来看两个样本延误时间差异
flights %>% 
  mutate(
    cancelled=is.na(dep_time),
    sched_hour=sched_dep_time %/% 100,
    sched_min=sched_dep_time %% 100,
    sched_dep_time=sched_hour+sched_min/60
  ) %>% 
  ggplot(mapping = aes(x=sched_dep_time,y=..density..,color=cancelled))+
  geom_freqpoly(binwidth=1/4)+
  geom_density(alpha=.7)

flights %>% 
  mutate(
    cancelled=is.na(dep_time),
    sched_hour=sched_dep_time %/% 100,
    sched_min=sched_dep_time %% 100,
    sched_dep_time=sched_hour+sched_min/60
  ) %>% 
  ggplot(mapping = aes(x=origin,y=sched_dep_time))+
  geom_boxplot()

diamonds %>%
  count(color, cut) %>% #相当于groupby+summarise
  ggplot(mapping = aes(x=color,y=cut,fill=n))+
  geom_tile()#两个变量组合观测数量

diamonds %>%
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11)) # 首先画散点图后发现x集中在4-11，集中在4-11，所以用coord_cartesian函数来限定一下，显得图形更直观
ggsave("diamonds.pdf") #保存到PDF，write_csv(diamonds, "diamonds.csv") 保存到csv

faithful %>% 
  ggplot(mapping = aes(x=eruptions,y=waiting))+
  geom_point()

readxl::read_xls("trial_class.xls", col_names = FALSE) # col_names = FALSE不要将第一行作为列 标题

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS")) #处理字符编码

planes %>% 
  count(tailnum) %>% 
  filter(n>1)

#Generate dummy dataset
# set seed
set.seed(1212) #由于是随机data，所以需要seed来记录
# fake data
df <- data_frame(
  name = ch_name(30),#ch_name(30) 随机创建30个假名字
  country = rep(c("US", "UK", "CN"), 10) %>% sample(), #rep函数，限制随机重复次数
  job = sample(ch_job(3), 30, replace = TRUE), #ch_job(3) 创建3个假job，sample随机打乱
  spending = rnorm(30, mean = 100, sd = 20), #随机一组平均值为100，标准差为20的30个数据
  item = sample(1:3, 30, replace = TRUE) #sample 随机1-3 出现30次
)

glimpse(df) #横向查看数据

# common tools
df %>% 
  filter(country=="US") %>% 
  mutate(per_item_spending=spending/item) %>% 
  group_by(job) %>% 
  summarise(total_spending=sum(spending),
            max_item = max(item),
            per_item_mu = mean(per_item_spending)
            ) %>% 
  arrange(total_spending)

# use select drop column
df %>% 
  #select(-country,-job)
  #select(-c(country, job))
  select(item, spending, name)

df %>% rename(person = name, 
              amount = spending, 
              quantity = item)

df %>% select(contains("ing")) # 选择包含ing列

df %>% 
  select(one_of("name", "item")) 

# use function from other package
df %>% 
  # from stringr package
  mutate(first_name = str_extract(name, "^\\w*"),
         last_name = str_extract(name, "\\w*$")) %>% 
  select(contains("name"))

# using if-else
df %>% 
  mutate(one_item = ifelse(item == 1, "Yes", "No")) %>% # ifelse 加入判断
  select(contains("item"))

# using case-when
df %>% 
  mutate(
    spending_cat = case_when(
      spending > 100 ~ "above 100",
      spending > 50 ~ "above 50",
      TRUE ~ "below 50"
    )
  ) %>% 
  select(contains("spending"))
  
# filter multiple conditions
df %>%
  filter(country == "CN", item != 1, spending >= 100) #多条件筛选 

# find spending mean
df %>% 
  summarise(spending_mean = mean(spending),spending_sd=sd(spending))

# combine group by with filter or mutate
df %>% 
  group_by(job) %>% 
  filter(spending < mean(spending)) %>% 
  mutate(cumsum_spending = cumsum(spending)) 

k <- 2:10
k %>% map_dbl(sqrt)
# map()       # 返回一个列表（list）
# map_lgl()    # 返回一个逻辑型向量
# map_int()    # 返回一个整数型向量
# map_dbl()   # 返回双精度数值向量
# map_chr()   # 返回字符串向量
