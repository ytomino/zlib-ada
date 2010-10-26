-- test for deflating and inflating
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with zlib;
procedure test_di is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Offset;
begin
	Ada.Text_IO.Put_Line ("zlib version " & zlib.Version);
	declare
		Source_File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open (
			Source_File,
			Ada.Streams.Stream_IO.In_File,
			"test_di.adb");
		declare
			Size : constant Ada.Streams.Stream_IO.Count :=
				Ada.Streams.Stream_IO.Size (Source_File);
			Source : Ada.Streams.Stream_Element_Array
				(1 .. Ada.Streams.Stream_Element_Offset (Size));
			Reading_Last : Ada.Streams.Stream_Element_Offset;
		begin
			Ada.Streams.Stream_IO.Read (Source_File, Source, Reading_Last);
			Ada.Streams.Stream_IO.Close (Source_File);
			declare
				Gz_File : Ada.Streams.Stream_IO.File_Type;
				Stream : zlib.Stream := zlib.Deflate_Init (Header => zlib.GZip);
				First : Ada.Streams.Stream_Element_Offset := Source'First;
				In_Last : Ada.Streams.Stream_Element_Offset;
				Output : Ada.Streams.Stream_Element_Array (1 .. 10);
				Out_Last : Ada.Streams.Stream_Element_Offset;
				Finished : Boolean;
			begin
				Ada.Streams.Stream_IO.Create (
					Gz_File,
					Name => "test_di.gz");
				loop
					Ada.Text_IO.Put ('*');
					zlib.Deflate (
						Stream,
						Source (First .. Source'Last),
						In_Last,
						Output,
						Out_Last,
						Finish => True,
						Finished => Finished);
					Ada.Streams.Stream_IO.Write (
						Gz_File,
						Output (Output'First .. Out_Last));
					exit when Finished;
					First := In_Last + 1;
				end loop;
				Ada.Streams.Stream_IO.Close (Gz_File);
				Ada.Text_IO.New_Line;
			end;
			declare
				Gz_File : Ada.Streams.Stream_IO.File_Type;
				Stream : zlib.Stream := zlib.Inflate_Init (Header => zlib.GZip);
				Input : Ada.Streams.Stream_Element_Array (1 .. 10);
				In_First : Ada.Streams.Stream_Element_Offset := Input'Last + 1;
				In_Last : Ada.Streams.Stream_Element_Offset;
				In_Used : Ada.Streams.Stream_Element_Offset;
				Output : Ada.Streams.Stream_Element_Array (1 .. 10);
				Out_Last : Ada.Streams.Stream_Element_Offset;
				Finished : Boolean;
				Index : Ada.Streams.Stream_Element_Offset := Source'First;
			begin
				Ada.Streams.Stream_IO.Open (
					Gz_File,
					Ada.Streams.Stream_IO.In_File,
					Name => "test_di.gz");
				loop
					Ada.Text_IO.Put ('*');
					if In_First > Input'Last then
						Ada.Streams.Stream_IO.Read (Gz_File, Input, In_Last);
						In_First := Input'First;
					end if;
					zlib.Inflate (
						Stream,
						Input (In_First .. In_Last),
						In_Used,
						Output,
						Out_Last,
						Finish => In_Last < Input'Last,
						Finished => Finished);
					for I in Output'First .. Out_Last loop
						if Output (I) /= Source (Index) then
							raise Program_Error;
						end if;
						Index := Index + 1;
					end loop;
					exit when Finished;
					In_First := In_Used + 1;
				end loop;
				Ada.Streams.Stream_IO.Close (Gz_File);
				Ada.Text_IO.New_Line;
				if Index /= Source'Last + 1 then
					raise Program_Error;
				end if;
			end;
		end;
	end;
end test_di;
