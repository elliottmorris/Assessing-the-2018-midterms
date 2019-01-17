### this file makes figures for the presentation slides

source("~/setup_elliott.R")


# average seat change -----------------------------------------------------

df <- read.csv("data/seat_change.csv",stringsAsFactors = F) %>% 
  filter(year>1970,!is.na(seatchg))

head(df)

df <- df %>%
  mutate(seatchg = ifelse(party=="Republican",seatchg*-1,seatchg*1),
         senate_seatchg =  ifelse(party=="Republican",senate_seatchg*-1,senate_seatchg*1))


# house
gg <- ggplot(df,
       aes(y=as.character(year),x=seatchg,col=party)) +
  geom_lollipop(horizontal =T,size=1) +
  geom_label(aes(label=seatchg),show.legend = F) +
  labs(title="Midterms seat change in the House of Representatives",
       x="Change in Democratic Seats",
       y="") +
  scale_color_manual("White House Party", values=c("Democrat"="#3498DB","Republican"="#E74C3C")) +
  theme_elliott()

plot_elliott(gg,"figures/house_change.png",height = 1100,width=1200,unit='px',res=170,themearg = theme(legend.position = 'top'))

# house
gg <- ggplot(df,
       aes(y=as.character(year),x=senate_seatchg,col=party)) +
  geom_lollipop(horizontal =T,size=1) +
  geom_label(aes(label=seatchg),show.legend = F) +
  labs(title="Midterms seat change in the Senate",
       x="Change in Democratic Seats",
       y="") +
  scale_color_manual("White House Party", values=c("Democrat"="#3498DB","Republican"="#E74C3C")) +
  theme_elliott() + theme(legend.position = 'top')

plot_elliott(gg,"figures/senate_change.png",height = 1100,width=1200,unit='px',res=170,themearg = theme(legend.position = 'top'))


# vote by demogs ----------------------------------------------------------

df <- read.csv("data/vote_margins.csv",stringsAsFactors = F)

gg <- ggplot(df) +
  annotate('text',x=-33,y='White non-college',label='2016→',size=6) +
  annotate('text',x=-21,y='White non-college',label='←2018',col= '#3498DB',size=6)+
  geom_segment(aes(y=as.character(demographic),
                   yend=as.character(demographic),
                   x=margin.2016,xend=margin.2018)) + 
  geom_point(aes(y=as.character(demographic),x=margin.2016)) +
  geom_point(aes(y=as.character(demographic),x=margin.2018),col='#3498DB') +
  geom_label(aes(y=as.character(demographic),x=margin.2016,label=margin.2016),show.legend = F,nudge_x = -1) +
  geom_label(aes(y=as.character(demographic),x=margin.2016,label=margin.2016),show.legend = F,nudge_x = -1) +
  geom_label(aes(y=as.character(demographic),x=margin.2018,label=margin.2018),show.legend = F,nudge_x = 1,col='#3498DB') +
  labs(title="White Americans swung left, especially women\nand the college-educated",
       x="Democratic Vote Margin",
       y="",
       caption="Source: Catalist") +
  theme_elliott() 


plot_elliott(gg,"figures/demographics.png",height = 900,width=1500,unit='px',res=170)



# seat-level: rural america -----------------------------------------------
df <- read.csv("data/house_master.csv",stringsAsFactors = F)

df <- df %>%
  mutate(density_type = factor(density_type,
                               levels=c("Pure rural","Rural-suburban mix","Sparse suburban","Urban-suburban mix","Dense suburban","Pure urban")))

gg <- ggplot(df %>%
         filter(uncontested==0,uncontested_2016==0,!grepl("PA|LA",district))) +
  geom_vline(xintercept=0,linetype=2) +
  geom_segment(aes(y=reorder(district,dem.house.margin.2018),
                   yend=reorder(district,dem.house.margin.2018),
                   x=clinton.margin.2016,
                   xend=dem.house.margin.2018,
                   col=dem.house.margin.2018>clinton.margin.2016),
               arrow = arrow(length=unit(0.3,"cm"))) +
  facet_wrap(~density_type,nrow=1,scales = 'free_y') +
  labs(title='Where House Democrats Beat Hillary Clinton',
       x = "Arrow drawn from Clinton's 2016 margin to Democrats' 2018 House margin",
       y="District",
       caption="Density classification from CityLab") +
  scale_color_manual(values=c("TRUE"="blue","FALSE"="red")) +
  scale_x_continuous(labels=scales::percent_format())

plot_elliott(gg,"figures/density_dems.png",height = 1400,width=2400,unit='px',res=170)



# mean reversion ----------------------------------------------------------

gg <- ggplot(df %>%
         filter(uncontested==0,too_early==0,uncontested_2016==0,!grepl("PA|LA|WV",district)),
       aes(x=swing_obama_clinton,
           y=dem.house.margin.2018-clinton.margin.2016)) +
  geom_text(aes(label=district),alpha=0.8) +
  geom_smooth(method='loess',se=F,linetype=2,size=1.2) +
  labs(title='Democrats did Better in Seats that Swung Toward Trump in 2016',
       subtitle="But Republicans didn't really revert from their losses to Clinton",
       x= "Swing from Obama to Clinton",
       y="Swing from Clinton to House Democrats in 2018")  +
  scale_x_continuous(labels=scales::percent_format(1)) +
  scale_y_continuous(labels=scales::percent_format(1))

plot_elliott(gg,"figures/mean_reversion.png",height = 1000,width=1400,unit='px',res=170)



# seat-level: white ba ----------------------------------------------------
df <- read.csv("data/house_master.csv",stringsAsFactors = F)

