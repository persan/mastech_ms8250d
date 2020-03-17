pragma Ada_2012;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Ada.Real_Time;
package body Ms8250d is
   use Ada.Real_Time;
   ----------
   -- Open --
   ----------

   overriding procedure Open
     (Port : out Ms8250d_Device; Name : GNAT.Serial_Communications.Port_Name)
   is
   begin
      GNAT.Serial_Communications.Serial_Port (Port).Open (Name);
      Port.Set (Rate      => GNAT.Serial_Communications.B2400,
                Bits      => GNAT.Serial_Communications.CS8,
                Stop_Bits => GNAT.Serial_Communications.One,
                Parity    => GNAT.Serial_Communications.None);
   end Open;

   ----------
   -- Read --
   ----------

   use Ada.Streams;

   procedure Read
     (Port : not null access Ms8250d_Device; Data : out Ms8250d_Info'Class)
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 18) := (others => 0);
      Last   : Ada.Streams.Stream_Element_Offset;
      T      : Ada.Real_Time.Time;
      Cursor : Ada.Streams.Stream_Element_Offset;
      Trig   : constant Ada.Real_Time.Time_Span := To_Time_Span (0.06);

   begin
      loop
         Cursor := Buffer'First;
         loop
            T := Ada.Real_Time.Clock;
            Port.Read (Buffer (Cursor .. Cursor), Last);
            exit when  Ada.Real_Time.Clock  - T > Trig;
         end loop;
         Cursor := Cursor + 1;
         loop
            Port.Read (Buffer (Cursor .. Cursor), Last);
            exit when Cursor = Buffer'Last;
            Cursor := Cursor + 1;
         end loop;
         if Cursor = Buffer'Last then
            Data.Parse (Buffer);
            return;
         end if;
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   function Read (Port : not null access Ms8250d_Device) return Ms8250d_Info'Class is
   begin
      return Ret : Ms8250d_Info do
         Port.Read (Ret);
      end return;
   end Read;

   -----------
   -- Value --
   -----------

   function Value (Device :  Ms8250d_Info) return Long_Float is
   begin
      return Device.Main_Display;
   end Value;

   type Unsigned is new Interfaces.C.unsigned;
   --  Bar_Display at 0 range 0 .. 15;
   Is_Ac          : constant := 8 * 1 + 4;
   Is_Dc          : constant := 8 * 2 + 1;
   Is_Auto        : constant := 8 * 16 + 4;
   Is_Rs232       : constant := 8 * 1 + 1;
   Is_Micro       : constant := 8 * 8 + 4;
   Is_Micro_Fd    : constant := 8 * 9 + 1;
   Is_Nano        : constant := 8 * 8 + 5;
   Is_Kilo        : constant := 8 * 9 + 2;
   Is_Diode       : constant := 8 * 11 + 0;
   Is_Milli       : constant := 8 * 9 + 0;
   --  Is_Percent  : constant := 8* 0 + 0..0;
   Is_Mega        : constant := 8 * 8 + 6;
   Is_Beep        : constant := 8 * 11 + 1;
   Is_Farad       : constant := 8 * 10 + 1;
   Is_Ohm         : constant := 8 * 9 + 6;
   Is_Rel         : constant := 8 * 15 + 7;
   Is_Hold        : constant := 8 * 16 + 3;
   Is_Ampere      : constant := 8 * 10 + 0;
   Is_Volt        : constant := 8 * 9 + 4;
   Is_Hz          : constant := 8 * 10 + 2;
   Is_Bat         : constant := 8 * 1 + 5;
   Is_Ncv         : constant := 8 * 0 + 0;
   Is_Min         : constant := 8 * 16 + 2;
   Is_Max         : constant := 8 * 16 + 1;
   --      Is_Sign     : constant := 8* 0 + 0;
   Is_Autotimer   : constant := 8 * 1 + 0;
   --     Sec_Exponent_1 : constant := 8 * 12 + 7;
   --     Sec_Exponent_2 : constant := 8 * 13 + 7;
   --     Sec_Exponent_3 : constant := 8 * 14 + 7;
   --     Sec_Is_Signed  : constant := 8 * 0  + 2;

   type Packed_Boolean_Array is array (Natural range <>) of Boolean with Pack => True;

   function Parse_Digit2 (Val : Ada.Streams.Stream_Element) return Long_Float is
   begin
      case Val is
         when 16#00# => return 0.0;
         when 16#7D# => return 0.0;
         when 16#05# => return 1.0;
         when 16#1B# => return 2.0;
         when 16#1F# => return 3.0;
         when 16#27# => return 4.0;
         when 16#3E# => return 5.0;
         when 16#7E# => return 6.0;
         when 16#15# => return 7.0;
         when 16#7F# => return 8.0;
         when 16#3F# => return 9.0;
         when others => return raise Constraint_Error with "Invalid Value" & Val'Img;
      end case;
   end;

   function Parse_Digit (Val : Unsigned) return Long_Float is
   begin
      case Val is
         when 2#0000_0000_0000# => return 0.0;
            --  when 2#0100_0011_0000# => return -1.0;
         when 2#0101_0011_0011# => return 0.0;
         when 2#0000_0000_0011# => return 1.0;
         when 2#0111_0010_0001# => return 2.0;
         when 2#0111_0000_0011# => return 3.0;
         when 2#0010_0001_0011# => return 4.0;
         when 2#0111_0001_0010# => return 5.0;
         when 2#0111_0011_0010# => return 6.0;
         when 2#0001_0000_0011# => return 7.0;
         when 2#0111_0011_0011# => return 8.0;
         when 2#0111_0001_0011# => return 9.0;
         when others => return raise Constraint_Error with "Invalid Value" & Val'Img;
      end case;
   end;

   procedure Parse
     (Meter : in out Ms8250d_Info; Data : Ada.Streams.Stream_Element_Array)
   is
      Map      : Packed_Boolean_Array  (0 .. Data'Size - 1) with
        Import => True,
        Address => Data'Address;
   begin
      if Data'Length /= 18 then
         raise Constraint_Error with "invalid data";
      end if;
      Meter.Is_Ac := Map (Is_Ac);
      Meter.Is_Dc := Map (Is_Dc);
      Meter.Is_Auto := Map (Is_Auto);
      Meter.Is_Rs232 := Map (Is_Rs232);
      Meter.Is_Nano := Map (Is_Nano);
      Meter.Is_Kilo := Map (Is_Kilo);
      Meter.Is_Diode := Map (Is_Diode);
      Meter.Is_Milli := Map (Is_Milli);
      Meter.Is_Mega := Map (Is_Mega);
      Meter.Is_Beep := Map (Is_Beep);
      Meter.Is_Farad := Map (Is_Farad);
      Meter.Is_Ohm := Map (Is_Ohm);
      Meter.Is_Rel := Map (Is_Rel);
      Meter.Is_Hold := Map (Is_Hold);
      Meter.Is_Ampere := Map (Is_Ampere);
      Meter.Is_Volt := Map (Is_Volt);
      Meter.Is_Hz := Map (Is_Hz);
      Meter.Is_Bat := Map (Is_Bat);
      Meter.Is_Ncv := Map (Is_Ncv);
      Meter.Is_Min := Map (Is_Min);
      Meter.Is_Max := Map (Is_Max);
      Meter.Is_Autotimer := Map (Is_Autotimer);
      Meter.Is_Micro := Map (Is_Micro_Fd) or else Map (Is_Micro);

      --  Get display value
      Meter.Main_Display  :=
        (Parse_Digit (((Unsigned (Data (Data'First + 7)) and 2#0111_0011#) * (2 ** 4)) or (Unsigned (Data (Data'First + 8)) and 2#0000_0011#)) * 1.0) +
        (Parse_Digit (((Unsigned (Data (Data'First + 6)) and 2#0000_0111#) * (2 ** 8)) or (Unsigned (Data (Data'First + 5)) and 2#0011_0000#) or ((Unsigned (Data (Data'First + 6)) and 2#0011_0000#) / (2 ** 4)))) * 10.0 +
        (Parse_Digit (((Unsigned (Data (Data'First + 4)) and 2#0111_0011#) * (2 ** 4)) or (Unsigned (Data (Data'First + 5)) and 2#0000_0011#))) * 100.0 +
        (Parse_Digit (((Unsigned (Data (Data'First + 3)) and 2#0000_0111#) * (2 ** 8)) or (Unsigned (Data (Data'First + 2)) and 2#0011_0000#) or ((Unsigned (Data (Data'First + 3)) and 2#0011_0000#) / (2 ** 4)))) * 1000.0;

      --  Get decimal point
      Meter.Main_Display := Meter.Main_Display * (if (Data (Data'First + 3) and 2#0100_0000#) /= 0 then
                                                     0.001
                                                  elsif (Data (Data'First + 5) and 2#0100_0000#) /= 0 then
                                                     0.01
                                                  elsif (Data (Data'First + 7) and 2#0100_0000#) /= 0 then
                                                     0.1
                                                  else
                                                     1.0);
      --  Get sign
      Meter.Main_Display := Meter.Main_Display *
        (if (Data (Data'First + 0) and 2#0000_0100#)  /= 0 then -1.0 else 1.0);

      Meter.Secondary_Display :=
        (Parse_Digit2 (Data (Data'First + 12) and 2#0111_1111#) * 1.0) +
        (Parse_Digit2 (Data (Data'First + 13) and 2#0111_1111#) * 10.0) +
        (Parse_Digit2 (Data (Data'First + 14) and 2#0111_1111#) * 100.0) +
        (Parse_Digit2 (Data (Data'First + 15) and 2#0111_1111#) * 1000.0);

      Meter.Secondary_Display := Meter.Secondary_Display *
        (if (Data (Data'First + 14) and 2#1000_0000#) /= 0 then
            0.001
         elsif (Data (Data'First + 13) and 2#1000_0000#) /= 0 then
            0.01
         elsif (Data (Data'First + 12) and 2#1000_0000#) /= 0 then
            0.1
         else
            1.0);

   end Parse;

   -----------
   -- Image --
   -----------

   function Image (Meter : Ms8250d_Info) return String is
   begin
      return
        "Main_Display => " & Meter.Main_Display'Img & " " &

      (if Meter.Is_Micro then "u"
       elsif Meter.Is_Milli then "m"
       elsif Meter.Is_Nano then "n"
       elsif Meter.Is_Kilo then "k"
       elsif Meter.Is_Mega then "M"
       else "") &

      (if Meter.Is_Farad then "Fd"
       elsif Meter.Is_Ohm then "Ohm"
       elsif Meter.Is_Ampere then "A"
       elsif Meter.Is_Volt then "V"
       elsif Meter.Is_Hz then "Hz"
       else "") &
        "  Secondary_Display => " & Meter.Secondary_Display'Img;
   end Image;

end Ms8250d;
