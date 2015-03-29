import Signal (Signal, (<~), (~), map)
import Text (asText)
import Time (fps)

main = asText <~ map (\t -> t / 20) (fps 15)
