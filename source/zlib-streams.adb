package body zlib.Streams is
	
	Buffer_Length_Table : constant array (Direction) of
		Ada.Streams.Stream_Element_Count := (Reading => 2 ** 15, Writing => 0);
	
	-- implementation
	
	function Create_Deflation (
		Direction : Streams.Direction := Writing;
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Level : Compression_Level := Default_Compression;
		Method : Compression_Method := Deflated;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Deflation_Header := Default;
		Memory_Level : zlib.Memory_Level := Default_Memory_Level;
		Strategy : zlib.Strategy := Default_Strategy)
		return Stream
	is
		pragma Suppress (Accessibility_Check);
	begin
		return (Ada.Streams.Root_Stream_Type with
			Buffer_Length => Buffer_Length_Table (Direction),
			Direction => Direction,
			Target => Target,
			Raw => Deflate_Init (
				Level => Level,
				Method => Method,
				Window_Bits => Window_Bits,
				Header => Header,
				Memory_Level => Memory_Level,
				Strategy => Strategy),
			In_Buffer => <>,
			In_First => 0,
			In_Last => -1);
	end Create_Deflation;
	
	function Create_Inflation (
		Direction : Streams.Direction := Reading;
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Inflation_Header := Auto)
		return Stream
	is
		pragma Suppress (Accessibility_Check);
	begin
		return (Ada.Streams.Root_Stream_Type with
			Buffer_Length => Buffer_Length_Table (Direction),
			Direction => Direction,
			Target => Target,
			Raw => Inflate_Init (
				Window_Bits => Window_Bits,
				Header => Header),
			In_Buffer => <>,
			In_First => 0,
			In_Last => -1);
	end Create_Inflation;
	
	procedure Finish (Object : in out Stream) is
		Dummy_In_Item : Ada.Streams.Stream_Element_Array (1 .. 0);
		Dummy_In_Used : Ada.Streams.Stream_Element_Offset;
		Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
		Out_Last : Ada.Streams.Stream_Element_Offset;
		Finished : Boolean;
	begin
		loop
			Deflate_Or_Inflate (
				Object.Raw,
				Dummy_In_Item,
				Dummy_In_Used,
				Out_Item,
				Out_Last,
				True,
				Finished);
				if Object.Direction = Writing then
					Ada.Streams.Write (
						Object.Target.all,
						Out_Item (Out_Item'First .. Out_Last));
				end if;
			exit when Finished;
		end loop;
	end Finish;
	
	overriding procedure Read (
		Object : in out Stream;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset) is
	begin
		if Object.Direction /= Reading then
			raise Status_Error;
		elsif End_Of (Object.Raw) then
			Last := Item'First - 1;
		else
			declare
				In_Used : Ada.Streams.Stream_Element_Offset;
				Out_First : Ada.Streams.Stream_Element_Offset := Item'First;
				Finish : Boolean := False;
				Finished : Boolean;
			begin
				loop
					if Object.In_First > Object.In_Last then
						Ada.Streams.Read (
							Object.Target.all,
							Object.In_Buffer,
							Object.In_Last);
						Object.In_First := Object.In_Buffer'First;
						if Status (Object.Raw) = Deflating then
							Finish := Finish
								or else Object.In_Last < Object.In_Buffer'Last;
						end if;
					end if;
					Deflate_Or_Inflate (
						Object.Raw,
						Object.In_Buffer (Object.In_First .. Object.In_Last),
						In_Used,
						Item (Out_First .. Item'Last),
						Last,
						Finish,
						Finished);
					Object.In_First := In_Used + 1;
					exit when Finished or else Last >= Item'Last;
					Out_First := Last + 1;
				end loop;
			end;
		end if;
	end Read;
	
	overriding procedure Write (
		Object : in out Stream;
		Item : in Ada.Streams.Stream_Element_Array) is
	begin
		if Object.Direction /= Writing then
			raise Status_Error;
		elsif Item'First <= Item'Last then
			declare
				In_First : Ada.Streams.Stream_Element_Offset := Item'First;
				In_Used : Ada.Streams.Stream_Element_Offset;
				Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
				Out_Last : Ada.Streams.Stream_Element_Offset;
				Finished : Boolean;
			begin
				loop
					Deflate_Or_Inflate (
						Object.Raw,
						Item (In_First .. Item'Last),
						In_Used,
						Out_Item,
						Out_Last,
						False,
						Finished);
					Ada.Streams.Write (
						Object.Target.all,
						Out_Item (Out_Item'First .. Out_Last));
					exit when Finished or else In_Used >= Item'Last;
					In_First := In_Used + 1;
				end loop;
			end;
		end if;
	end Write;
	
	-- compatibility
	
	procedure Close (Stream : in out Streams.Stream'Class) is
	begin
		Close (Stream.Raw);
	end Close;
	
	procedure Create (
		Stream : in out Streams.Stream'Class;
		Mode : in Stream_Mode;
		Back : in Stream_Access;
		Back_Compressed : in Boolean;
		Level : in Compression_Level := Default_Compression;
		Strategy : in Strategy_Type := Default_Strategy;
		Header : in Header_Type := Default) is
	begin
		if Mode = In_Stream and then Stream.Buffer_Length = 0 then
			raise Constraint_Error;
		else
			Stream.Direction := Mode;
			Stream.Target := Back;
			Stream.In_First := 0;
			Stream.In_Last := -1;
			if (Mode = Out_Stream) = Back_Compressed then
				Deflate_Init (
					Stream.Raw,
					Level => Level,
					Header => Header,
					Strategy => Strategy);
			else
				Inflate_Init (
					Stream.Raw,
					Header => Header);
			end if;
		end if;
	end Create;
	
	procedure Flush (
		Stream : in out Streams.Stream'Class;
		Mode : in Flush_Mode) is
	begin
		if Mode then
			Finish (Stream);
		end if;
	end Flush;
	
	function Read_Total_In (Stream : Streams.Stream'Class) return Count is
	begin
		return Total_In (Stream.Raw);
	end Read_Total_In;
	
	function Read_Total_Out (Stream : Streams.Stream'Class) return Count is
	begin
		return Total_Out (Stream.Raw);
	end Read_Total_Out;
	
	function Write_Total_In (Stream : Streams.Stream'Class) return Count
		renames Read_Total_Out;
	
	function Write_Total_Out (Stream : Streams.Stream'Class) return Count
		renames Read_Total_Out;
	
end zlib.Streams;
