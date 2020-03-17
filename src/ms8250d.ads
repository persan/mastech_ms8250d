with Ada.Streams;
with GNAT.Serial_Communications;
package Ms8250d is

   type Ms8250d_Device is new GNAT.Serial_Communications.Serial_Port with null record;
   overriding
   procedure Open (Port : out Ms8250d_Device; Name : GNAT.Serial_Communications.Port_Name);

   type Ms8250d_Info is tagged record
      Is_Ac          : Boolean := False;
      Is_Dc          : Boolean := False;
      Is_Auto        : Boolean := False;
      Is_Rs232       : Boolean := False;
      Is_Micro       : Boolean := False;
      Is_Nano        : Boolean := False;
      Is_Kilo        : Boolean := False;
      Is_Diode       : Boolean := False;
      Is_Milli       : Boolean := False;
      --  Is_Percent   : Boolean;
      Is_Mega        : Boolean := False;
      Is_Beep        : Boolean := False;
      Is_Farad       : Boolean := False;
      Is_Ohm         : Boolean := False;
      Is_Rel         : Boolean := False;
      Is_Hold        : Boolean := False;
      Is_Ampere      : Boolean := False;
      Is_Volt        : Boolean := False;
      Is_Hz          : Boolean := False;
      Is_Bat         : Boolean := False;
      Is_Ncv         : Boolean := False;
      Is_Min         : Boolean := False;
      Is_Max         : Boolean := False;
      --      Is_Sign      : Boolean;
      Is_Autotimer   : Boolean := False;

      Main_Display        : Long_Float := 0.0;
      Secondary_Display   : Long_Float := 0.0;
   end record;

   procedure Read (Port : not null access Ms8250d_Device; Data : out Ms8250d_Info'class);
   function Read (Port : not null access Ms8250d_Device) return Ms8250d_Info'Class;
   function Value (Device :  Ms8250d_Info) return Long_Float;

   procedure Parse (Meter : in out Ms8250d_Info; Data : Ada.Streams.Stream_Element_Array)
     with Pre => Data'Length = 18;

   function Image (Meter :  Ms8250d_Info) return String;

end Ms8250d;
