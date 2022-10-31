open System
open System.Drawing
open System.Windows.Forms
open System.IO

open emulator

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


let exercisePaint (machine: Chip8) (e : PaintEventArgs) =
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


let exerciseProcessBatch (machine: Chip8, exercise: Form) (e: EventArgs ) =
   try
      machine.runOne()
      if machine.Display.Modified
      then
         exercise.Invalidate()
         machine.Display.Modified <- false
   with
      | BadOperation(str) -> printfn "Bad operation: %s" str
      | _ -> printfn "Unknown error"


let forceInvalidate (exercise: Form) (e: EventArgs ) =
   exercise.Invalidate()


let mutable worker: Threading.Thread = null
let mutable workerActive: bool = false

let workerThreadLoop(machine: Chip8) () =
   while workerActive do
      try
         machine.runOne()
      with
         | BadOperation(str) -> printfn "Bad operation: %s" str
         | _ -> printfn "Unknown error"


let startWorkerThread (machine: Chip8) =
   worker <- new Threading.Thread(new Threading.ThreadStart( workerThreadLoop(machine) ))
   workerActive <- true
   worker.Start()

let stopWorkerThread (e: EventArgs) =
   if worker <> null
   then
      workerActive <- false
      worker.Join()
      worker <- null


let bootMachine (machine: Chip8) =
   let rom = loadRom( "chip8rom/IbmLogo.ch8" )
   machine.loadIntoMemory rom 512us
   machine.jump(512us)

let exercise = new Form(Size = new Size(640, 360), MaximizeBox = true, Text = "Exercise")
let mutable keyboard = Chip8Keyboard()
let mutable display = Chip8Display()
let mutable machine = Chip8(display, keyboard)
exercise.Paint.Add (exercisePaint machine)
exercise.KeyDown.Add (exerciseKeyDown keyboard)
exercise.KeyUp.Add (exerciseKeyUp keyboard)
exercise.Closing.Add (stopWorkerThread)

bootMachine machine

// let mutable timer = new System.Windows.Forms.Timer()
// timer.Interval <- 1
// timer.Tick.Add(exerciseProcessBatch (machine, exercise))
// timer.Start()
startWorkerThread machine
let mutable timer = new System.Windows.Forms.Timer()
timer.Interval <- 100
timer.Tick.Add(forceInvalidate (exercise))
timer.Start()

do Application.Run exercise
