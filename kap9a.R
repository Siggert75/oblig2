"
X∼bin(20,0.375). 

Lag tabell over sannsynligheter forx= 0,....,20, og plott både pdf og CDF for denne sannsynlighetsfordelingen. 
Regn deretter ut E[X],Var(X), og P(2< X <7)
"

x = c(0:20)

y = dbinom(x, 20, 0.375)

#png(file = "dbinom1.png")
library(ggplot2)

df = data.frame(x, y)
#plot(x, y, type="l")
r = ggplot(df, aes(x = x, y = y)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+geom_point()
#dev.off()
r + labs(x = "Størrelse", y = "Sannsynlighet")

ggplot(data=df, mapping=aes(x=x, y=y)) +
  geom_line()+
  geom_area(mapping=aes(x=ifelse(x>=2 & x<=7, x, 0)), fill="#9898fb", alpha=1.) +
  xlim(0,20) +
  ylim(0, max(y)) + 
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)


y2 = pbinom(x, 20, 0.375)

df2 = data.frame(x, y2)
#plot(x, y2)

r2 = ggplot(df2, aes(x = x, y = y2)) + geom_smooth(method = "gam", formula = y2 ~ s(x, bs = "cs"))+geom_point()
r2 + labs(x = "Størrelse", y = "Sannsynlighet")

var(y)

sum(x*0.375)

gg = pbinom(7, length(x), 0.375)
gg2 = pbinom(2, length(x), 0.375)

print(gg-gg2)

