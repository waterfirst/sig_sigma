
library(tidyverse)

df <- tibble::tribble(
  ~두께,     ~경화, ~PT, ~경화율_1635,
  10L,    "N2", "X",      0.99,
  10L,    "O2", "O",      0.98,
  10L,     "X", "X",      0.92,
  8L,     "X", "X",       0.9,
  10L,    "N2", "X",         1,
  8L,    "N2", "X",      0.98,
  8L, "N2_O2", "O",      0.98,
  8L,     "X", "X",      0.89
) |> mutate_if(is.character, as.factor)


library(psych)

headTail(df)

df <- df |> mutate(두께 = as.factor(두께))

str(df)

summary(df)




interaction.plot(x.factor     = df$두께,
                 trace.factor = df$경화,

                 response     = df$경화율_1635,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")



model = lm(경화율_1635 ~ 두께+경화 ,
           data = df)


model2 = lm(경화율_1635 ~ 두께+PT ,
           data = df)

model3 = lm(경화율_1635 ~ 경화+PT ,
            data = df)

df

library(phia)

IM = interactionMeans(model)
IM2 = interactionMeans(model2)
IM3 = interactionMeans(model3)

dev.off()

IM
IM2



### Produce interaction plot

plot(IM, main = "Main Effect Plot")
plot(IM2, main = "Main Effect Plot")


### Return the graphics device to its default 1-plot-per-window state

par(mfrow=c(1,1))
