module emulator

open System
open System.IO

exception BadOperation of string

// https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
// Chip8Display - array of bytes, initialized to 0
type Chip8Display() =
      let mutable pixels = Array.create (64 / 8 * 32 ) 0uy
      let mutable modified = false

      member this.Pixels
         with get() = &pixels

      member this.clear() =
         pixels <- Array.create (64 / 8 * 32 ) 0uy

      member this.Modified
         with get() = &modified


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
   Dn.Modified <- true
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

      member this.loadIntoMemory (bytes: byte[]) (toAddress: uint16) =
         bytes.CopyTo( this.Bytes, int toAddress )

type Chip8Bios() =
      let font: byte array = [|
         0xF0uy; 0x90uy; 0x90uy; 0x90uy; 0xF0uy; // 0
         0x20uy; 0x60uy; 0x20uy; 0x20uy; 0x70uy; // 1
         0xF0uy; 0x10uy; 0xF0uy; 0x80uy; 0xF0uy; // 2
         0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy; // 3
         0x90uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy; // 4
         0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy; // 5
         0xF0uy; 0x80uy; 0xF0uy; 0x90uy; 0xF0uy; // 6
         0xF0uy; 0x10uy; 0x20uy; 0x40uy; 0x40uy; // 7
         0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy; // 8
         0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0xF0uy; // 9
         0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy; // A
         0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy; // B
         0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy; // C
         0xE0uy; 0x90uy; 0x90uy; 0x90uy; 0xE0uy; // D
         0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy; // E
         0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy  // F
      |]

      // Use a commonly used location where the font is stored in memory.
      // It can be stored in any location between 0x0000 and 0x01ff.
      let fontAddress = 0x0050us

      member this.FontAddress
         with get() = fontAddress

      member this.loadFont (memory: Chip8Memory) =
         memory.loadIntoMemory font fontAddress

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

      member this.highestPressedKey state =
         if state = (uint16 0) then None
         else
            let rec highBit count bits =
               match int(bits &&& uint16 0x80) with
                  | 0 -> highBit (count - 1) (bits <<< 1)
                  | _ -> Some( byte count )
            highBit 15 state


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
   let bios = Chip8Bios()
   do
      bios.loadFont memory

   member this.Display with get() = &display

   member this.drawSprite( rx, ry, numRows ) =
      let x0 = registers.V[int rx] % 64uy
      let y0 = registers.V[int ry] % 32uy
      let nr = min (32uy - y0) numRows
      // log $"drawSprite %d{x0} %d{y0} %d{numRows} (-> %d{nr})"
      for dy in 0uy .. (nr - 1uy) do
         display <- updateChip8Display display (int x0) (int(y0 + dy)) memory.Bytes[(int index) + (int dy)]

   member this.fontChar rx = 
      index <- bios.FontAddress + uint16(((int rx) &&& 0x0f) * 5)
      // NOTE: &&& 0x0f - COSMAC VIP uses only the low nibble for the index -> max 16 characters

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

   member this.getKey rx = 
      match keyboard.highestPressedKey keyboard.getState  with
         | Some( keyIndex )  -> registers.V[int rx] <- keyIndex
         | None -> programCounter <- programCounter - 2us
      // NOTE: COSMAC VIP: also waits for the key to be released

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
   
   member this.runOne() =
      let op = this.fetch()
      this.execute( int ((op &&& 0xf000us) >>> 8 ), int (op &&& 0x0fffus) )

   member this.demoRun() =
      this.runOne()
      if programCounter < 552us  // TODO: remove; IbmLogo infinite loop start
      then this.demoRun()

   member this.loadIntoMemory (bytes: byte[]) (toAddress: uint16) =
      memory.loadIntoMemory bytes toAddress

let loadRom( filename ) =
   use stream = File.Open(filename, FileMode.Open, FileAccess.Read)
   use mem = new MemoryStream()
   stream.CopyTo mem
   mem.ToArray()
