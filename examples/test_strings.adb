with Ada.Command_Line;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with zlib.Strings;
procedure test_strings is
	use type Ada.Streams.Stream_Element_Offset;
	Verbose : Boolean := False;
	Text : constant String := "Hello, zlib!";
	Buffer : Ada.Streams.Stream_Element_Array (0 .. 1023);
	Buffer_Last : Ada.Streams.Stream_Element_Offset := Buffer'First - 1;
	Extracted : String (1 .. 1024);
	Extracted_Last : Natural := Extracted'First - 1;
begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		if Ada.Command_Line.Argument (I) = "-v" then
			Verbose := True;
		end if;
	end loop;
	declare
		Stream : zlib.Stream := zlib.Deflate_Init (Header => zlib.GZip);
		Text_Last : Natural := Text'First - 1;
		Finished : Boolean;
	begin
		loop
			zlib.Strings.Deflate (
				Stream,
				Text (Text_Last + 1 .. Text'Last),
				Text_Last,
				Buffer (Buffer_Last + 1 .. Buffer'Last),
				Buffer_Last,
				Finish => True,
				Finished => Finished);
			exit when Finished;
		end loop;
	end;
	declare
		Stream : zlib.Stream := zlib.Inflate_Init (Header => zlib.GZip);
		Infl_Buffer_Last : Ada.Streams.Stream_Element_Offset := Buffer'First - 1;
		Finished : Boolean;
	begin
		loop
			zlib.Strings.Inflate (
				Stream,
				Buffer (Infl_Buffer_Last + 1 .. Buffer_Last),
				Infl_Buffer_Last,
				Extracted (Extracted_Last + 1 .. Extracted'Last),
				Extracted_Last,
				Finish => True,
				Finished => Finished);
			exit when Finished;
		end loop;
	end;
	declare
		use Ada.Text_IO, Ada.Integer_Text_IO;
		Compressed_Length : constant Natural :=
			Integer (Buffer_Last - Buffer'First + 1);
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
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_strings;
