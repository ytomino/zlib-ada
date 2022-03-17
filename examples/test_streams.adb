with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with zlib.Streams;
procedure test_streams is
	Verbose : Boolean := False;
	Text : constant String := "Hello, zlib!";
	Temporary_File : Ada.Streams.Stream_IO.File_Type;
	Extracted : String (1 .. 1024);
	Extracted_Last : Natural;
begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		if Ada.Command_Line.Argument (I) = "-v" then
			Verbose := True;
		end if;
	end loop;
	Ada.Streams.Stream_IO.Create (Temporary_File); -- temporary file
	declare
		Deflator : zlib.Streams.Out_Type :=
			zlib.Streams.Open (
				Ada.Streams.Stream_IO.Stream (Temporary_File),
				Header => zlib.GZip);
	begin
		String'Write (zlib.Streams.Stream (Deflator), Text);
		zlib.Streams.Finish (Deflator);
	end;
	Ada.Streams.Stream_IO.Reset (Temporary_File, Ada.Streams.Stream_IO.In_File);
	declare
		Inflator : zlib.Streams.In_Type :=
			zlib.Streams.Open (
				Ada.Streams.Stream_IO.Stream (Temporary_File),
				Header => zlib.GZip);
	begin
		Extracted_Last := Text'Length;
		String'Read (zlib.Streams.Stream (Inflator), Extracted (1 .. Extracted_Last));
		begin
			String'Read (
				zlib.Streams.Stream (Inflator),
				Extracted (Extracted_Last + 1 .. Extracted'Last));
			raise Program_Error; -- bad
		exception
			when zlib.Streams.End_Error => null;
		end;
	end;
	declare
		use Ada.Text_IO, Ada.Integer_Text_IO;
		Compressed_Length : constant Natural :=
			Integer (Ada.Streams.Stream_IO.Size (Temporary_File));
		Extracted_Length : constant Natural := Extracted_Last - Extracted'First + 1;
	begin
		if Verbose then
			Ada.Integer_Text_IO.Default_Width := 0;
			Put ("source length     = ");
			Put (Text'Length);
			New_Line;
			Put ("compressed length = ");
			Put (Compressed_Length);
			New_Line;
			Put ("extracted length  = ");
			Put (Extracted_Length);
			New_Line;
		end if;
		pragma Assert (Extracted (Extracted'First .. Extracted_Last) = Text);
	end;
	Ada.Streams.Stream_IO.Close (Temporary_File);
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_streams;
