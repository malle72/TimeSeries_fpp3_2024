# Chapter 3 Exercises

# 3
# Why is a Box-Cox transformation unhelpful for the canadian_gas data?

canadian_gas |> autoplot(Volume)

cg_lambda <- canadian_gas |>
  features(Volume, features = guerrero)|>
  pull(lambda_guerrero)

canadian_gas |>
  autoplot(box_cox(Volume,cg_lambda))

dcmp <- canadian_gas |>
  model(classical_decomposition(Volume))

components(dcmp) |>
  autoplot()
