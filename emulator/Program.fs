open System
open System.Drawing
open System.Windows.Forms

type Chip8Display() =
      let mutable pixels = Array.create (64 / 8 * 32 ) 0uy
      member this.Length
         with get() = pixels.Length
      member this.Pixels
         with get() = &pixels

// https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
// Chip8Display - array of bytes, initialized to 0
let mutable gChip8DisplayData = Chip8Display()
printf $"{gChip8DisplayData.Length}\n"

let renderChip8Display (G: Drawing.Graphics) x0 y0 (D: Chip8Display) =
   let pixSize = 9
   let pixGap = 1
   let drawPixel x y brush =
      G.FillRectangle(brush, new Rectangle( x * pixSize, y * pixSize,
         pixSize - pixGap, pixSize - pixGap ))
      // e.Graphics.DrawLine(pnBorder, new Point(5, 10), new Point(55, 10 ))

   let brushOn = new SolidBrush( Color.Red )
   let brushOff = new SolidBrush( Color.Blue )

   for i in 0 .. D.Length - 1 do
      let x = x0 + (i % (64 / 8)) * 8
      let y = y0 + i / (64 / 8)
      for shift in 0 .. 7 do
         let on = D.Pixels[i] &&& (1uy <<< (7 - shift))
         if on <> 0uy then
            drawPixel (x + shift) y brushOn
         else
            drawPixel (x + shift) y brushOff

let updateChip8Display (D : Chip8Display) x y pix8 =
   let x0 = x % 64
   let xoffs = x0 / 8
   let shift = x0 % 8
   let y0 = y % 32
   let offs = y0 * 64 / 8 + xoffs
   let mutable Dn = D
   // TODO (mmahnic): The bits have to be x-ored
   Dn.Pixels[offs] <- Dn.Pixels[offs] ||| (pix8 >>> shift)
   if shift > 0 && x0 < 63 then
      Dn.Pixels[offs+1] <- Dn.Pixels[offs+1] ||| (pix8 <<< (8 - shift))
   Dn

let drawTestSprite display =
    let mutable d2 = updateChip8Display display 0 0 0x31uy
    d2 <- updateChip8Display d2 1 1 0x51uy
    d2 <- updateChip8Display d2 2 2 0x31uy
    d2 <- updateChip8Display d2 3 3 0x51uy
    d2 <- updateChip8Display d2 2 4 0x31uy
    d2 <- updateChip8Display d2 1 5 0x51uy
    d2 <- updateChip8Display d2 0 6 0x31uy
    d2

let exercisePaint(e : PaintEventArgs) =
    renderChip8Display e.Graphics 0 0 (drawTestSprite gChip8DisplayData) |> ignore

let exercise = new Form(Size = new Size(640, 360), MaximizeBox = true, Text = "Exercise")
exercise.Paint.Add exercisePaint
do Application.Run exercise