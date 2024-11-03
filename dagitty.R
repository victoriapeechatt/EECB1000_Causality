library(dagitty)
library(ggdag)

?dagitty
dseparated(x, y)

g <- dagitty("dag {
  X <-> Y
  X -> a X -> b X -> c X -> d
  Y -> a Y -> b Y -> c Y -> d
}")

ggdag::ggdag_classic()

dag <- dagify(
  y ~ x + z2 + w2 + w1,
  x ~ z1 + w1 + w2,
  z1 ~ w1 + v,
  z2 ~ w2 + v,
  exposure = "x",
  outcome = "y"
) %>%
  tidy_dagitty()

ggdag_classic(dag)
ggdag_classic(dag) + theme_dag_blank()

ggdag_adjustment_set(dag, node_size = 14)
ggdag_dconnected(dag, from = "x",to = "y", controlling_for = "z")

dag2 <- dagify(
  y ~ a,
  a ~ x,
  a ~ c + b,
  exposure = x,
  outcome = y
)

ggdag_classic(dag2)
ggdag_classic(dag2) + theme_dag_blank()
ggdag_adjustment_set(dag2)
ggdag_dconnected(dag2, from = "x",to = "y",controlling_for = "a")

dag3 <- dagify(
  y ~ z, 
  z ~ x
)

ggdag_classic(dag3)
ggdag_classic(dag3) + theme_dag_blank()
ggdag_adjustment_set(dag2)
ggdag_dconnected(dag3, from = "x",to = "y",controlling_for = "z")
