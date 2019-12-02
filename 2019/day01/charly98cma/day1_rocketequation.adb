with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Command_Line;

procedure day1_rocketequation is
   
   -- Package renaming, because it's easier for everyone :)
   package IIO renames Ada.Integer_Text_IO;
   package IO renames Ada.Text_IO;
   package CL renames Ada.Command_Line;
   
   -- Variables and constants
   Sum, TMP_Val, TMP_Val_2 : Integer := 0;
   N_Args : constant Integer := CL.Argument_Count;
   MYNAME : constant String := "day1_rocketequation";
   Input : IO.File_Type;
   
begin
   
   -- Check for number of arguments (just in case)
   if N_Args = 0 then
      IO.Put_Line("Use: ./" & MYNAME & " values.txt");
      return; -- Abort execution
   end if;
   
   -- Opening the file with our precious values
   IO.Open(File => Input,
	   Mode => IO.In_File,
	   Name => CL.Argument(1));
   
   loop
      -- Reading each number and doing calculations
      IIO.Get(File => Input,
	      Item => TMP_Val);
      loop
	 TMP_Val_2 := TMP_Val/3 - 2;
	 exit when TMP_Val_2 <= 0;
	 Sum := Sum + TMP_Val_2;
	 TMP_Val := TMP_Val_2;
      end loop;
   end loop;
   
exception
   -- EOF, time to give the result :D
   when IO.End_Error =>
      IO.Put_Line("The total fuel required is ..." & Integer'Image(Sum));
end day1_rocketequation;
