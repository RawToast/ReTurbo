/**
 * 
 * Simple distance calculator.
 * Standard distance = always 80
 * From Y 0 = height 80
 * From Y 40 = height 70?
 * From Y 80 = height 60?
 * From Y 140 = height 45
 * From Y 320 = height 0
 * 
 */
let yDistance = distance => { 
  let yDelta = 320 / 80;
  distance / yDelta 
}

let calcX = yDistance => yDistance *. tan(40.);
let calcHypotenuse = yDistance => 0. -. (yDistance /. cos(40.));
