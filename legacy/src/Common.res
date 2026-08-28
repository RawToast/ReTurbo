let height = 320
let width = 568
let heightF = float_of_int(height)
let widthF = float_of_int(width)
let centrePoint = widthF /. 2.
let centreHeight = heightF /. 2.
let horizon = heightF /. 3. *. 2.

let minOffset = -2.
let maxOffset = 2.
let roadWidth = 600.
let frameRate = 60.

let planes = 100

let cameraDepth = 1. /. tan(80. /. 2. *. 3.1459)
