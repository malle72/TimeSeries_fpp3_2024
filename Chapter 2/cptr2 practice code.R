

PBS |> 
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost)
