//
//    Ice Cream Machine - An emulator for the J1a CPU
//    Copyright (C) 2016  Matthias Koch
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

uses crt;

// Configuration of the emulator

const dstacksize = 32;
      rstacksize = 32;

// Number output helpers as Base cannot be changed in Pascal :-)

function byte2hex(zahl : byte) : string;
const
    hexa : array [0..15] of char = '0123456789ABCDEF';
begin
  byte2hex := hexa[zahl shr 4] + hexa[zahl and 15];
end;

function word2hex(zahl : word) : string;
begin
  word2hex := Byte2Hex((zahl and $FF00) shr 8) + Byte2Hex(zahl and $00FF);
end;

// IO Definitions

var ticks : word = 0;
    keypending : boolean = true;  // For initialisation, there is a "character" in the UART data buffer after Reset in the real hardware.
    currentkey : byte = 0;
    loading    : boolean = false; // To get input from a source code file instead.
    sourcecode : file of byte;

procedure writeio(addr, data : word);
begin
  // writeln(word2hex(data), ' ', word2hex(addr), ' io!');
  if (addr and $1000) <> 0 then write(chr(data and $FF));  // Send character over UART
  if (addr and $4000) <> 0 then ticks := 0;               // Clear cycle counter
end;

function readio(addr : word) : word;
var ortogether : word = 0;
begin

  if (addr and $1000) <> 0 then // Fetch UART data buffer
    if keypending then keypending := false
    else
    begin
      if loading then
      begin
        currentkey := 0;
        if eof(sourcecode) then loading := false else read(sourcecode, currentkey);
      end
      else currentkey := ord(readkey);

      ortogether := ortogether or currentkey;
    end;

  if (addr and $2000) <> 0 then // Random, SPI, UART state...
  begin
    ortogether := ortogether or $000F;
    if random(2) = 1 then ortogether := ortogether or $0020;
  end;

  if (addr and $4000) <> 0 then // Cycle counter
    ortogether := ortogether or ticks;

  //writeln(word2hex(addr), ' io@ ', word2hex(ortogether) );
  readio := ortogether;
end;

// Processor state, stacks and memory

var pc : word = 0;
    depth : word = 0;
   rdepth : word = 0; // Not available in J1a, but useful for debugging
    dstack : array[0..dstacksize-1] of word;
    rstack : array[0..rstacksize-1] of word;
    memory : array[0..$1FFF] of word;

(*

procedure printstate; // Just for debugging. uses math
var k : integer;
begin
  write (word2hex(pc shl 1), ' : ', word2hex(memory[pc]));
  write (' D: <', byte2hex(depth), '> ');
  for k := min(depth-1, dstacksize-1) downto 0 do write(word2hex(dstack[k]), ' ');
  write (' R: <', byte2hex(rdepth), '> ');
  for k := min(rdepth-1, rstacksize-1) downto 0 do write(word2hex(rstack[k]), ' ');
  writeln ('*>');
end;

*)

// Load memory image for startup

function char2nibble(c : char) : word;
begin
  case c of
    '0'..'9': char2nibble := ord(c) - 48;
    'A'..'F': char2nibble := ord(c) - 65 + 10;
    'a'..'f': char2nibble := ord(c) - 97 + 10;
  else
    writeln ('Invalid input character ', c); halt;
  end;
end;

function hex2word(data : string) : word;
begin
  hex2word := char2nibble(data[1]) shl 12 or
              char2nibble(data[2]) shl  8 or
              char2nibble(data[3]) shl  4 or
              char2nibble(data[4]);
end;

procedure loadimage(name : string);
var image : text;
    addr : word = 0;
    row : string;
begin
  fillchar(memory, sizeof(memory), 0);
  assign(image, name);
  reset(image);

  while not eof(image) do
  begin
    readln(image, row);
    memory[addr] := hex2word(row);
    inc(addr);
  end;

  close(image);
end;

// Save memory image for core generation

procedure saveimage(name : string);
var image : text;
    addr : word;
begin
  assign(image, name);
  rewrite(image);
  for addr := 0 to $1FFF do writeln(image, word2hex(memory[addr]));
  close(image);
end;

// Small helpers for Stacks and Flags

    function n(content : word) : boolean;
    begin
      n := (content and $8000) <> 0;
    end;

    function flag(yesorno : boolean) : word;
    begin
      if yesorno then flag := $FFFF else flag := 0;
    end;

    function signed(data : word) : longint;
    begin
      if (data and $8000) = 0 then signed := data
                              else signed := 0 - ((not(data) + 1) and $FFFF);
    end;

    procedure dpush(content : word);
    var k : integer;
    begin
      for k := dstacksize-2 downto 0 do dstack[k+1] := dstack[k];
      dstack[0] := content;
      depth := (depth + 1) and $1F;
    end;

    function dpop : word;
    var k : integer;
    begin
      dpop := dstack[0];
      for k := 0 to dstacksize-2 do dstack[k] := dstack[k + 1];
      dstack[dstacksize-1] := $55AA;
      depth := (depth - 1) and $1F;
    end;

    procedure rpush(content : word);
    var k : integer;
    begin
      for k := rstacksize-2 downto 0 do rstack[k+1] := rstack[k];
      rstack[0] := content;
      rdepth := (rdepth + 1) and $1F;
    end;

    function rpop : word;
    var k : integer;
    begin
      rpop := rstack[0];
      for k := 0 to rstacksize-2 do rstack[k] := rstack[k + 1];
      rstack[rstacksize-1] := $55AA;
      rdepth := (rdepth - 1) and $1F;
    end;

    function arshift(data, howfar : word) : word;
    var k : word;
    begin
      if howfar > 0 then
      begin
        for k := 1 to howfar do
          if n(data) then data := (data shr 1) or $8000 else data := data shr 1;
      end;
      arshift := data;
    end;

// Emulate a single instruction and update the state

procedure cpu_step;

var insn, tos, nos, rtos, tosN : word;

begin
  ticks := (ticks + 1) and $FFFF; // Cycle counter is available as IO register.

  insn := memory[pc and $1FFF]; // High-Call = Memory Fetch, mask away the topmost address bit.

  if (pc and $2000) <> 0 then begin dpush(insn); pc := rpop shr 1; end                   // Memory fetch
  else
  begin
    if (insn and $8000) <> 0 then begin dpush(insn and $7FFF); inc(pc); end              // Literal
    else
    begin
      case insn and $E000 of
        $0000: begin                          pc := insn and $1FFF;                end;  // Jump
        $2000: begin         if dpop = 0 then pc := insn and $1FFF else inc(pc);   end;  // Conditional jump
        $4000: begin  rpush((pc + 1) shl 1);  pc := insn and $1FFF;                end;  // Call
        $6000: begin                                                                     // ALU

                rtos := rstack[0];
                 tos := dstack[0];
                 nos := dstack[1];

                 case insn and $1F00 of
                   $0000: tosN := tos;
                   $0100: tosN := nos;
                   $0200: tosN := tos +   nos;
                   $0300: tosN := tos and nos;

                   $0400: tosN := tos or  nos;
                   $0500: tosN := tos xor nos;
                   $0600: tosN := not(tos);
                   $0700: tosN := flag( nos = tos );

                   $0800: tosN := flag( signed(nos) < signed(tos) );
                   $0900: tosN := (tos shr 1) or (tos and $8000);
                   $0A00: tosN :=  tos shl 1;
                   $0B00: tosN := rtos;

                   $0C00: tosN := nos - tos;
                   $0D00: tosN := readio(tos);
                   $0E00: tosN := depth;
                   $0F00: tosN := flag( nos < tos );

                   $1000: tosN := nos shl tos;
                   $1100: tosN := nos shr tos;
                   $1200: tosN := arshift(nos, tos);
                   $1300: tosN := rdepth;

                   $1400: tosN := (tos * nos) and $FFFF;
                   $1500: tosN := (tos * nos) shr 16;
                   $1600: tosN := tos + 1;
                   $1700: tosN := tos - 1;
                 end;

                 // ALU exit bit
                 if (insn and $0080) <> 0 then pc := rtos shr 1 else inc(pc);

                 // Data stack movement
                 if (insn and $0003) = $0003 then       dpop;              // d-1
                 if (insn and $0003) = $0002 then begin dpop; dpop; end;  // d-2
                 if (insn and $0003) = $0001 then       dpush(tos);      // d+1

                 // Return stack movement
                 if (insn and $000C) = $000C then       rpop;              // r-1
                 if (insn and $000C) = $0008 then begin rpop; rpop; end;  // r-2
                 if (insn and $000C) = $0004 then       rpush(tos);      // r+1

                 // Read & Write operations
                 if (insn and $0070) = $0010 then dstack[1] := tos;             // T --> N
                 if (insn and $0070) = $0020 then rstack[0] := tos;            // T --> R
                 if (insn and $0070) = $0030 then memory[tos shr 1] := nos;   // Memory write
                 if (insn and $0070) = $0040 then writeio(tos, nos);         // IO write

                 dstack[0] := tosN;

               end; // ALU
      end;
    end;
  end;
end;


begin

  if (paramcount <> 1) and (paramcount <> 3) then
  begin
    writeln('Ice Cream Machine - An emulator for Mecrisp-Ice by Matthias Koch');
    writeln('Usage:');
    writeln('  icecreammachine image                       for interactive terminal');
    writeln('  icecreammachine image-in image-out source   for binary generation');
    halt;
  end;

  randomize;
  loadimage(paramstr(1));

  loading := paramcount = 3;

  if loading then
  begin
    assign(sourcecode, paramstr(3));
    reset(sourcecode);
    repeat cpu_step; until not loading; // Exit when source code file is exhausted
    close(sourcecode);
    saveimage(paramstr(2));
  end
  else repeat cpu_step; until currentkey = 27; // Exit when ESC is pressed.

end.
