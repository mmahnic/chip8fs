open System
open System.Drawing
open System.Windows.Forms
open System.IO

exception BadOperation of string

// https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
// Chip8Display - array of bytes, initialized to 0
type Chip8Display() =
      let mutable pixels = Array.create (64 / 8 * 32 ) 0uy
      member this.Pixels
         with get() = &pixels

      member this.clear() =
         pixels <- Array.create (64 / 8 * 32 ) 0uy


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
         let on = D.Pixels[i] &&& (0x80uy >>> shift)
         if on <> 0uy then
            drawPixel (x + shift) y brushOn
         else
            drawPixel (x + shift) y brushOff

let log s = 
      let appender = File.AppendText( "debug.log" )
      appender.WriteLine( s.ToString() )
      appender.Close() |> ignore

let updateChip8Display (D : Chip8Display) x y pix8 =
   let xorbits oldVal mask newVal =
      let changed = (oldVal &&& mask) ^^^ newVal
      let kept = oldVal &&& ~~~mask
      kept ||| changed

   let x0 = x % 64
   let xoffs = x0 / 8
   let shift = x0 % 8
   let y0 = y % 32
   let offs = y0 * 64 / 8 + xoffs
   let mutable Dn = D

   // log $"(%d{x}, %d{y}) -> (%d{x0} / %d{shift}, %d{y0}) -> offs %d{offs}"

   Dn.Pixels[offs] <- xorbits Dn.Pixels[offs] (0xffuy >>> shift) (pix8 >>> shift)
   if shift > 0 && x0 < 63 then
      Dn.Pixels[offs+1] <- xorbits Dn.Pixels[offs+1] (0xffuy <<< (8 - shift)) (pix8 <<< (8 - shift))
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
      let mutable top = -1

      member this.Addresses
         with get() = &addresses

      member this.Top
         with get() = addresses[top]

      member this.push v = 
         top <- top + 1
         addresses[top] <- v

      member this.pop = top <- top - 1

      member this.topandpop = 
         let v = addresses[top]
         top <- top - 1
         v


type Chip8Registers() =
      let mutable registers = Array.create (16) 0uy
      member this.V with get() = &registers
      member this.Flag with get() = registers[0x0f] and set(v) = registers[0x0f] <- v

      member this.set (reg, nn) =
         registers[int reg] <- nn

      member this.add (reg, nn) =
         registers[int reg] <- registers[int reg] + nn


type Chip8Keyboard() =
      let mutable keyStates = uint16 0x00

      member this.pressed keyIndex =
         keyStates &&& (uint16 0x01 <<< (keyIndex &&& 0x0f)) = uint16 0

      member this.setPressed keyIndex =
         keyStates <- keyStates ||| (uint16 0x01 <<< (keyIndex &&& 0x0f))

      member this.setReleased keyIndex =
         keyStates <- keyStates &&& (~~~ (uint16 0x01 <<< (keyIndex &&& 0x0f)))

      member this.getState = keyStates

      member this.lowestPressedKey state =
         if state = (uint16 0) then None
         else
            let rec lowbit count bits =
               match int(bits &&& uint16 0x01) with
                  | 0 -> lowbit (count + 1) (bits >>> 1)
                  | _ -> Some( byte count )
            lowbit 0 state


type Chip8(aDisplay: Chip8Display, aKeyboard: Chip8Keyboard) =
   let mutable display = aDisplay
   let mutable keyboard = aKeyboard
   let mutable memory = Chip8Memory()
   let mutable programCounter : uint16 = 0us
   let mutable index : uint16 = 0us
   let mutable addressStack = Chip8AddressStack()
   let mutable delayTimer : byte = 0uy
   let mutable soundTimer : byte = 0uy
   let mutable registers = Chip8Registers()
   let mutable randomGenerator = Random()

   member this.Display with get() = &display

   member this.drawSprite( rx, ry, numRows ) =
      let x0 = registers.V[int rx] % 64uy
      let y0 = registers.V[int ry] % 32uy
      let nr = min (32uy - y0) numRows
      // log $"drawSprite %d{x0} %d{y0} %d{numRows} (-> %d{nr})"
      for dy in 0uy .. (nr - 1uy) do
         display <- updateChip8Display display (int x0) (int(y0 + dy)) memory.Bytes[(int index) + (int dy)]

   member this.fontChar rx = 
      // TODO (mmahnic): render character
      uint8 rx |> ignore 

   // Fetch 2 bytes at PC as an uint16 in big endian order and increase PC.
   member this.fetch() =
      let pc = int programCounter
      let op = (uint16 memory.Bytes[pc]) <<< 8 ||| (uint16 memory.Bytes[pc + 1])
      programCounter <- programCounter + 2us
      op

   member this.storeRegisters rx = 
      for i in 0 .. int rx do
         memory.Bytes[int index + i] <- registers.V[i]
      // NOTE: COSMAC VIP changed the index registry, other variants did not. We keep it constant.

   member this.loadRegisters rx = 
      for i in 0 .. int rx do
         registers.V[i] <- memory.Bytes[int index + i]
      // NOTE: COSMAC VIP changed the index registry, other variants did not. We keep it constant.

   member this.jump addr = programCounter <- addr

   member this.subJump addr =
         addressStack.push programCounter
         programCounter <- addr

   member this.offsJump addr = 
         // COSMAC VIP variant
         programCounter <- uint16 addr + uint16 registers.V[0]

   member this.subReturn = programCounter <- addressStack.topandpop

   member this.skipIfXnEq (rx, nn) =
         if registers.V[int rx] = nn then programCounter <- programCounter + 2us

   member this.skipIfXnNe (rx, nn) =
         if registers.V[int rx] <> nn then programCounter <- programCounter + 2us

   member this.skipIfXyEq (rx, ry) =
         if registers.V[int rx] = registers.V[int ry] then programCounter <- programCounter + 2us

   member this.skipIfXyNe (rx, ry) =
         if registers.V[int rx] <> registers.V[int ry] then programCounter <- programCounter + 2us

   member this.copyYtoX (rx, ry) =
         registers.V[int rx] <- registers.V[int ry]

   member this.orXy (rx, ry) =
         registers.V[int rx] <- registers.V[int rx] ||| registers.V[int ry]

   member this.andXy (rx, ry) =
         registers.V[int rx] <- registers.V[int rx] &&& registers.V[int ry]

   member this.xorXy (rx, ry) =
         registers.V[int rx] <- registers.V[int rx] ^^^ registers.V[int ry]

   member this.plusXy (rx, ry) =
         let res = int registers.V[int rx] + int registers.V[int ry]
         registers.Flag <- if res > 255 then 1uy else 0uy
         registers.V[int rx] <- byte res

   member this.minusXy (rx, ry) =
         let res = int registers.V[int rx] - int registers.V[int ry]
         registers.Flag <- if res < 0 then 0uy else 1uy
         registers.V[int rx] <- if registers.Flag = 0uy then byte res else byte (res + 256)

   member this.minusYx (rx, ry) =
         let res = int registers.V[int ry] - int registers.V[int rx]
         registers.Flag <- if res < 0 then 0uy else 1uy
         registers.V[int rx] <- if registers.Flag = 0uy then byte res else byte (res + 256)

   member this.rshiftXy (rx, ry) =
         // COSMAC VIP variant
         let vx = registers.V[int ry]
         registers.Flag <- vx &&& 0x01uy
         registers.V[int rx] <- vx >>> 1

   member this.lshiftXy (rx, ry) =
         // COSMAC VIP variant
         let vx = registers.V[int ry]
         registers.Flag <- (vx &&& 0x80uy) >>> 7
         registers.V[int rx] <- vx <<< 1

   member this.random (rx, nn) =
         let rand = randomGenerator.Next(256) |> byte
         registers.V[int rx] <- rand &&& nn

   member this.binaryCodedDec rx = 
         memory.Bytes[int index] <- byte rx / 100uy
         memory.Bytes[int index + 1] <- byte rx % 100uy / 10uy
         memory.Bytes[int index + 2] <- byte rx % 10uy
         // NOTE: We do not know if index should be increased. We keep it constant.

   member this.addIndex rx =
         index <- index + uint16 registers.V[int rx]
         // TODO (mmahnic): Amiga sets Flag when going from below 0x1000 to 0x1000 or above

   member this.skipIfKeyPressed rx = 
         if keyboard.pressed(int rx)
         then programCounter <- programCounter + 2us

   member this.skipIfKeyReleased rx = 
         if not (keyboard.pressed(int rx))
         then programCounter <- programCounter + 2us

   // wait for any key to go from "released" to "pressed"
   // COSMAC VIP: also waits for the key to be released
   member this.getKey rx = 
         // NOTE: this implementation is incorrect. The getKey instruction should be executed
         // repeatedly until a key is pressed.
         //    What does this mean:
         //      - ... stops execution and waits for key input.
         //      - If a key is pressed while this instruction is waiting for input, ...
         //    Is the condition: "any key is in pressed state" or "any key goes from unpressed
         //    to pressed state"?
         //    The current version implements the latter.
         let state = keyboard.getState 
         let mutable newState = keyboard.getState

         let getPressed olds news =
            let goneOn = ~~~olds &&& news
            keyboard.lowestPressedKey goneOn

         let mutable pressed = getPressed state keyboard.getState
         while pressed = None do
            System.Threading.Thread.Sleep(20)
            pressed <- getPressed state keyboard.getState

         registers.V[int rx] <- pressed.Value


   member this.getDelayTimer rx = 
         registers.V[int rx] <- delayTimer

   member this.setDelayTimer rx = 
         delayTimer <- registers.V[int rx]

   member this.setSoundTimer rx = 
         soundTimer <- registers.V[int rx]

   member this.execNumeric (rx, ry, op) =
      match int op with
         | 0x00 -> this.copyYtoX (rx, ry)
         | 0x01 -> this.orXy (rx, ry)
         | 0x02 -> this.andXy (rx, ry)
         | 0x03 -> this.xorXy (rx, ry)
         | 0x04 -> this.plusXy (rx, ry)
         | 0x05 -> this.minusXy (rx, ry)
         | 0x06 -> this.rshiftXy (rx, ry)
         | 0x07 -> this.minusYx (rx, ry)
         | 0x0e -> this.lshiftXy (rx, ry)
         | _ -> raise (BadOperation( "Unknown arithmetic operation" ))

   member this.execEx (rx: uint8, op: uint8) =
      match int op with 
         | 0x9e -> this.skipIfKeyPressed rx
         | 0xa1 -> this.skipIfKeyReleased rx
         | _ -> raise (BadOperation( "Unknown 0xExnn operation" ))

   member this.execFx (rx: uint8, op: uint8) =
      match int op with 
         | 0x07 -> this.getDelayTimer rx
         | 0x15 -> this.setDelayTimer rx
         | 0x18 -> this.setSoundTimer rx
         | 0x1e -> this.addIndex rx
         | 0x0a -> this.getKey rx
         | 0x29 -> this.fontChar rx
         | 0x33 -> this.binaryCodedDec rx
         | 0x55 -> this.storeRegisters rx
         | 0x65 -> this.loadRegisters rx
         | _ -> raise (BadOperation( "Unknown 0xFxnn operation" ))

   member this.execute( op, xyn ) =
      let getx xyn = (xyn &&& 0x0f00) >>> 8 |> uint8
      let gety xyn = (xyn &&& 0x00f0) >>> 4 |> uint8
      let getxy xyn = getx xyn, gety xyn
      let getn xyn = (xyn &&& 0x000f) |> uint8
      let getnn xyn = (xyn &&& 0x00ff) |> uint8
      let getnnn xyn = (xyn &&& 0x0fff) |> uint16
      let getxyn xyn = getx xyn, gety xyn, getn xyn
      let getxnn xyn = getx xyn, getnn xyn

      match op with
         | 0x00 -> 
            match xyn with
               | 0x0e0 -> display.clear()
               | 0x0ee -> this.subReturn
               | _ -> raise (BadOperation( "Unknown operation" ))
         | 0x10 -> getnnn xyn |> this.jump
         | 0x20 -> getnnn xyn |> this.subJump
         | 0x30 -> getxnn xyn |> this.skipIfXnEq
         | 0x40 -> getxnn xyn |> this.skipIfXnNe
         | 0x50 -> getxy xyn |> this.skipIfXyEq
         | 0x60 -> getxnn xyn |> registers.set
         | 0x70 -> getxnn xyn |> registers.add
         | 0x80 -> getxyn xyn |> this.execNumeric 
         | 0x90 -> getxy xyn |> this.skipIfXyNe
         | 0xa0 -> index <- getnnn xyn
         | 0xb0 -> getnnn xyn |> this.offsJump
         | 0xc0 -> getxnn xyn |> this.random
         | 0xd0 -> getxyn xyn |> this.drawSprite 
         | 0xe0 -> getxnn xyn |> this.execEx 
         | 0xf0 -> getxnn xyn |> this.execFx 
         | _ -> raise (BadOperation("Not yet"))
   
   member this.run() = 
      let op = this.fetch()
      this.execute( int ((op &&& 0xf000us) >>> 8 ), int (op &&& 0x0fffus) )
      if programCounter < 552us  // TODO: remove; IbmLogo infinite loop start
      then this.run()

   member this.loadIntoMemory( bytes: byte[], toAddress: uint16 ) =
      bytes.CopyTo( memory.Bytes, int toAddress )

type MachineThread() =
      let mutable machine = None

let loadRom( filename ) =
   use stream = File.Open(filename, FileMode.Open, FileAccess.Read)
   use mem = new MemoryStream()
   stream.CopyTo mem
   mem.ToArray()

let exercisePaint (machine: Chip8) (e : PaintEventArgs) =
   // let dir = Directory.GetCurrentDirectory() // exe directory by default
   let rom = loadRom( "chip8rom/IbmLogo.ch8" )
   machine.loadIntoMemory(rom, 512us)
   try
      machine.jump(512us)
      machine.run()
   with
      | BadOperation(str) -> printfn "Bad operation: %s" str
      | _ -> printfn "Unknown error"

   renderChip8Display e.Graphics 0 0 (machine.Display) |> ignore

let extractKey (e: KeyEventArgs) =
   match e.KeyCode with
      | Keys.D1 -> Some( 0 )
      | Keys.D2 -> Some( 1 )
      | Keys.D3 -> Some( 2 )
      | Keys.D4 -> Some( 3 )
      | Keys.Q -> Some( 4 )
      | Keys.W -> Some( 5 )
      | Keys.E -> Some( 6 )
      | Keys.R -> Some( 7 )
      | Keys.A -> Some( 8 )
      | Keys.S -> Some( 9 )
      | Keys.D -> Some( 0x0a )
      | Keys.F -> Some( 0x0b )
      | Keys.Y -> Some( 0x0c )
      | Keys.X -> Some( 0x0d )
      | Keys.C -> Some( 0x0e )
      | Keys.V -> Some( 0x0f )
      | _ -> None

let exerciseKeyDown (keyboard: Chip8Keyboard) (e : KeyEventArgs) =
   match extractKey( e ) with 
      | Some( x ) -> keyboard.setPressed x 
      | None -> ignore 0

let exerciseKeyUp (keyboard: Chip8Keyboard) (e : KeyEventArgs) =
   match extractKey( e ) with 
      | Some( x ) -> keyboard.setReleased x 
      | None -> ignore 0

let exercise = new Form(Size = new Size(640, 360), MaximizeBox = true, Text = "Exercise")
let mutable keyboard = Chip8Keyboard()
let mutable display = Chip8Display()
let mutable machine = Chip8(display, keyboard)
exercise.Paint.Add (exercisePaint machine)
exercise.KeyDown.Add (exerciseKeyDown keyboard)
exercise.KeyUp.Add (exerciseKeyUp keyboard)
do Application.Run exercise
