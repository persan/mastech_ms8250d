with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with Ada.Calendar;
--  with Ada.Directories;
with GNAT.Formatted_String;
with GNAT.Ctrl_C;

procedure Ms8250d.Main is
   use GNAT.Formatted_String;
   S      : aliased Ms8250d_Device;
   Continue : Boolean := True;
   procedure Handler is
   begin
      Continue := False;
      GNAT.Ctrl_C.Uninstall_Handler;
   end;
begin
   GNAT.Ctrl_C.Install_Handler (Handler'Unrestricted_Access);

   S.Open ("/dev/ttyUSB0");
   while Continue loop
      Put_Line (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%H:%M:%S") & ", " & "-" ("+" ("%f") & S.Read.Value));
      delay 1.0;
   end loop;
   S.Close;
end Ms8250d.Main;
