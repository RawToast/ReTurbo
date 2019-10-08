/**
 * 
 * Simple distance calculator.
 * Standard distance = always 80
 * From Y 0 = height 80
 * From Y 40 = height 70?
 * From Y 80 = height 60?
 * From Y 140 = height 45
 * From Y 160 = height 0
 * (sin (degrees *. pi /. 180.));;
 */
let pi = 4. *. atan(1.);
let toRadians = d => d *. pi /. 180.;
let seventyRadians = toRadians(70.);
let roadLength=80.;

let nextY = (currentY) => {
  if (currentY >= 320.) {
    currentY -. 80.
  } else {
    let yDelta = float_of_int(160 / 80); /* 2 */
    /* 160 == 160
    *  240 == 80
    *  320 == 0
    * 0 - (y - 320)
    */
    let revY = 0. -. (currentY -. 320.);
    let height = roadLength -. (revY /. yDelta);
    currentY -. height;
  }
}
// let fiDeg = degrees(50.);
let calcX = yDistance => yDistance /. tan(seventyRadians);

let calcRemX = yDistance => yDistance /. sin(seventyRadians);

