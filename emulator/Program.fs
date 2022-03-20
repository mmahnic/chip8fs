open System
open System.Drawing
open System.Windows.Forms
open System.IO

exception BadOperation of string

type Chip8Display() =
      let mutable pixels = Array.create (64 / 8 * 32 ) 0uy
      member this.Pixels
         with get() = &pixels

      member this.clear() =
         pixels <- Array.create (64 / 8 * 32 ) 0uy


// https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
// Chip8Display - array of bytes, initialized to 0
let mutable gChip8DisplayData = Chip8Display()
printf $"{gChip8DisplayData.Pixels.Length}\n"

let renderChip8Display (G: Drawing.Graphics) x0 y0 (D: Chip8Display) =
   let pixSize = 9
   let pixGap = 1
   let drawPixel x y brush =
      G.FillRectangle(brush, new Rectangle( x * pixSize, y * pixSize,
         pixSize - pixGap, pixSize - pixGap ))
      // e.Graphics.DrawLine(pnBorder, new Point(5, 10), new Point(55, 10 ))

   let brushOn = new SolidBrush( Color.Red )
   let brushOff = new SolidBrush( Color.Blue )

   for i in 0 .. D.Pixels.Length - 1 do
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

type Chip8Memory() =
      let mutable bytes = Array.create (4096) 0uy
      member this.Bytes
         with get() = &bytes

type Chip8AddressStack() =
      let mutable addresses = Array.create (12) 0us
      let mutable top = addresses.Length - 1
      member this.Addresses
         with get() = &addresses
      member this.Top
         with get() = addresses[top]

type Chip8Registers() =
      let mutable registers = Array.create (16) 0uy
      member this.V with get() = &registers
      member this.V0 with get() = registers[0x00] and set(v) = registers[0x00] <- v
      member this.V1 with get() = registers[0x01] and set(v) = registers[0x01] <- v
      member this.V2 with get() = registers[0x02] and set(v) = registers[0x02] <- v
      member this.V3 with get() = registers[0x03] and set(v) = registers[0x03] <- v
      member this.V4 with get() = registers[0x04] and set(v) = registers[0x04] <- v
      member this.V5 with get() = registers[0x05] and set(v) = registers[0x05] <- v
      member this.V6 with get() = registers[0x06] and set(v) = registers[0x06] <- v
      member this.V7 with get() = registers[0x07] and set(v) = registers[0x07] <- v
      member this.V8 with get() = registers[0x08] and set(v) = registers[0x08] <- v
      member this.V9 with get() = registers[0x09] and set(v) = registers[0x09] <- v
      member this.VA with get() = registers[0x0a] and set(v) = registers[0x0a] <- v
      member this.VB with get() = registers[0x0b] and set(v) = registers[0x0b] <- v
      member this.VC with get() = registers[0x0c] and set(v) = registers[0x0c] <- v
      member this.VD with get() = registers[0x0d] and set(v) = registers[0x0d] <- v
      member this.VE with get() = registers[0x0e] and set(v) = registers[0x0e] <- v
      member this.VF with get() = registers[0x0f] and set(v) = registers[0x0f] <- v
      member this.Flag with get() = registers[0x0f] and set(v) = registers[0x0f] <- v

type Chip8() =
   let mutable display = Chip8Display()
   let mutable memory = Chip8Memory()
   let mutable programCounter : uint16 = 0us
   let mutable index : uint16 = 0us
   let mutable addressStack = Chip8AddressStack()
   let mutable delayTimer : byte = 0uy
   let mutable soundTimer : byte = 0uy
   let mutable registers = Chip8Registers()

   member this.Display with get() = &display

   member this.drawSprite( rx, ry, numRows ) =
      let x0 = registers.V[int rx] % 64uy
      let y0 = registers.V[int ry] % 32uy
      let nr = min (32uy - y0) numRows
      for dy in 0uy .. (nr - 1uy) do
         display <- updateChip8Display display (int x0) (int(y0 + dy)) memory.Bytes[(int index) + (int dy)]

   // Fetch 2 bytes at PC as an uint16 in big endian order and increase PC.
   member this.fetch() =
      let pc = int programCounter
      let op = (uint16 memory.Bytes[pc]) <<< 8 ||| (uint16 memory.Bytes[pc + 1])
      programCounter <- programCounter + 2us
      op

   member this.execute( op, xyn ) =
      let getx xyn = (xyn &&& 0x0f00) >>> 8 |> uint8
      let gety xyn = (xyn &&& 0x00f0) >>> 4 |> uint8
      let getnn xyn = (xyn &&& 0x00ff) |> uint8
      let getnnn xyn = (xyn &&& 0x0fff) |> uint16

      match op with
         | 0x00 -> 
            if  xyn = 0x00e0 then display.clear() 
            else raise (BadOperation( "Unknown operation" ))
         | 0x10 -> programCounter <- getnnn xyn
         | 0x60 -> registers.V[getx xyn |> int] <- getnn xyn
         | 0x70 -> registers.V[getx xyn |> int] <- registers.V[getx xyn |> int] + getnn xyn
         | 0xa0 -> index <- getnnn xyn
         | 0xd0 -> this.drawSprite( getx xyn, gety xyn, getnn xyn )
         | _ -> raise (BadOperation("Not yet"))
   
   member this.jumpTo( address: uint16 ) = programCounter <- (address &&& 0x0fffus)

   member this.run() = 
      let op = this.fetch()
      this.execute( int ((op &&& 0xf000us) >>> 8 ), int (op &&& 0x0fffus) )
      if programCounter < 552us  // IbmLogo infinite loop start
      then this.run()

   member this.loadIntoMemory( bytes: byte[], toAddress: uint16 ) =
      bytes.CopyTo( memory.Bytes, int toAddress )

type MachineThread() =
      let mutable machine = Chip8()

let loadRom( filename ) =
   use stream = File.Open(filename, FileMode.Open, FileAccess.Read)
   use mem = new MemoryStream()
   stream.CopyTo mem
   mem.ToArray()

let exercisePaint(e : PaintEventArgs) =
   let mutable machine = Chip8()
   // let dir = Directory.GetCurrentDirectory() // exe directory by default
   let rom = loadRom( "chip8rom/IbmLogo.ch8" )
   machine.loadIntoMemory(rom, 512us)
   try
      machine.jumpTo(512us)
      machine.run()
   with
      | BadOperation(str) -> printfn "Bad operation: %s" str
      | _ -> printfn "Unknown error"

   renderChip8Display e.Graphics 0 0 (machine.Display) |> ignore

let exercise = new Form(Size = new Size(640, 360), MaximizeBox = true, Text = "Exercise")
exercise.Paint.Add exercisePaint
do Application.Run exercise
