import Text (asText)

data = 4

x2 x = 2 * x
x3 x = 3 * x

minus2 x = x - 2

pipe1 = minus2 >> x2 >> x3
pipe2 = x2 >> minus2 >> x3

main = data |> pipe1 |> asText
