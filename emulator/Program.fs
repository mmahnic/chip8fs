open System
open System.Drawing
open System.Windows.Forms

// Chip8Display - array of bytes, initialized to 0
let mutable Chip8DisplayData = Array.create (64 / 8 * 32 ) 0uy
printf $"{Chip8DisplayData.Length}\n"

let updateChip8Display (D : byte[]) x y pix =
   let x0 = x % 64
   let xoffs = x0 / 8
   let shift = x0 % 8
   let y0 = y % 32
   let offs = y0 * 64 / 8 + xoffs
   let mutable Dn = D
   // TODO (mmahnic): The bits have to be x-ored
   Dn[offs] <- Dn[offs] ||| (pix >>> shift)
   if shift > 0 && x0 < 63 then
      Dn[offs+1] <- Dn[offs+1] ||| (pix <<< (8 - shift))
   Dn

let renderChip8Display (G: Drawing.Graphics) x0 y0 (D: byte[]) =
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
         let on = D[i] &&& (1uy <<< (7 - shift))
         if on <> 0uy then
            drawPixel (x + shift) y brushOn
         else
            drawPixel (x + shift) y brushOff

let exercisePaint(e : PaintEventArgs) =
    let pnBorder : Pen = new Pen(Color.Red)
    pnBorder.Width <- 3.00F
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 0 0 0x31uy
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 1 1 0x31uy
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 2 2 0x31uy
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 3 3 0x31uy
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 2 4 0x31uy
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 1 5 0x31uy
    Chip8DisplayData <- updateChip8Display Chip8DisplayData 0 6 0x31uy
    renderChip8Display e.Graphics 0 0 Chip8DisplayData |> ignore

let exercise = new Form(Size = new Size(640, 320), MaximizeBox = true, Text = "Exercise")
exercise.Paint.Add exercisePaint
do Application.Run exercise