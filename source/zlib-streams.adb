package body zlib.Streams is
	
	generic
		with procedure Deflate_Or_Inflate (
			Stream : in out zlib.Stream;
			In_Item : in Ada.Streams.Stream_Element_Array;
			In_Last : out Ada.Streams.Stream_Element_Offset;
			Out_Item : out Ada.Streams.Stream_Element_Array;
			Out_Last : out Ada.Streams.Stream_Element_Offset;
			Finish : in Boolean;
			Finished : out Boolean);
	procedure Generic_Finish (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Object : in out zlib.Stream);
	
	procedure Generic_Finish (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Object : in out zlib.Stream)
	is
		Dummy_In_Item : Ada.Streams.Stream_Element_Array (1 .. 0);
		Dummy_In_Used : Ada.Streams.Stream_Element_Offset;
		Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
		Out_Last : Ada.Streams.Stream_Element_Offset;
		Finished : Boolean;
	begin
		loop
			Deflate_Or_Inflate (
				Object,
				Dummy_In_Item,
				Dummy_In_Used,
				Out_Item,
				Out_Last,
				True,
				Finished);
				Ada.Streams.Write (
					Stream.all,
					Out_Item (Out_Item'First .. Out_Last));
			exit when Finished;
		end loop;
	end Generic_Finish;
	
	generic
		with procedure Deflate_Or_Inflate (
			Stream : in out zlib.Stream;
			In_Item : in Ada.Streams.Stream_Element_Array;
			In_Last : out Ada.Streams.Stream_Element_Offset;
			Out_Item : out Ada.Streams.Stream_Element_Array;
			Out_Last : out Ada.Streams.Stream_Element_Offset;
			Finish : in Boolean;
			Finished : out Boolean);
	procedure Generic_Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Object : in out zlib.Stream;
		Reading_In_First : in out Ada.Streams.Stream_Element_Offset;
		Reading_In_Last : in out Ada.Streams.Stream_Element_Offset;
		Reading_In_Buffer : in out Ada.Streams.Stream_Element_Array);
	
	procedure Generic_Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Object : in out zlib.Stream;
		Reading_In_First : in out Ada.Streams.Stream_Element_Offset;
		Reading_In_Last : in out Ada.Streams.Stream_Element_Offset;
		Reading_In_Buffer : in out Ada.Streams.Stream_Element_Array) is
	begin
		if Reference (Object).Stream_End then
			Last := Item'First - 1;
		else
			declare
				In_Used : Ada.Streams.Stream_Element_Offset;
				Out_First : Ada.Streams.Stream_Element_Offset := Item'First;
				Finish : Boolean := False;
				Finished : Boolean;
			begin
				loop
					if Reading_In_First > Reading_In_Last then
						Reading_In_First := Reading_In_Buffer'First;
						begin
							Ada.Streams.Read (
								Stream.all,
								Reading_In_Buffer,
								Reading_In_Last);
						exception
							when End_Error =>
								Reading_In_Last := Reading_In_First - 1;
						end;
						if Reading_In_Last < Reading_In_Buffer'First then
							Finish := True;
						end if;
					end if;
					Deflate_Or_Inflate (
						Object,
						Reading_In_Buffer (Reading_In_First .. Reading_In_Last),
						In_Used,
						Item (Out_First .. Item'Last),
						Last,
						Finish,
						Finished);
					Reading_In_First := In_Used + 1;
					exit when Finished or else Last >= Item'Last;
					Out_First := Last + 1;
				end loop;
			end;
		end if;
		if Last = Item'First - 1 then
			raise End_Error;
		end if;
	end Generic_Read;
	
	generic
		with procedure Deflate_Or_Inflate (
			Stream : in out zlib.Stream;
			In_Item : in Ada.Streams.Stream_Element_Array;
			In_Last : out Ada.Streams.Stream_Element_Offset;
			Out_Item : out Ada.Streams.Stream_Element_Array;
			Out_Last : out Ada.Streams.Stream_Element_Offset;
			Finish : in Boolean;
			Finished : out Boolean);
	procedure Generic_Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in Ada.Streams.Stream_Element_Array;
		Object : in out zlib.Stream);
	
	procedure Generic_Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : in Ada.Streams.Stream_Element_Array;
		Object : in out zlib.Stream) is
	begin
		if Item'First <= Item'Last then
			declare
				In_First : Ada.Streams.Stream_Element_Offset := Item'First;
				In_Used : Ada.Streams.Stream_Element_Offset;
				Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
				Out_Last : Ada.Streams.Stream_Element_Offset;
				Finished : Boolean;
			begin
				loop
					Deflate_Or_Inflate (
						Object,
						Item (In_First .. Item'Last),
						In_Used,
						Out_Item,
						Out_Last,
						False,
						Finished);
					Ada.Streams.Write (
						Stream.all,
						Out_Item (Out_Item'First .. Out_Last));
					exit when Finished or else In_Used >= Item'Last;
					In_First := In_Used + 1;
				end loop;
			end;
		end if;
	end Generic_Write;
	
	-- only writing with deflation
	
	function Open (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Level : Compression_Level := Default_Compression;
		Method : Compression_Method := Deflated;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Deflation_Header := Default;
		Memory_Level : zlib.Memory_Level := Default_Memory_Level;
		Strategy : zlib.Strategy := Default_Strategy)
		return Out_Type
	is
		pragma Suppress (Accessibility_Check);
	begin
		return (Ada.Streams.Root_Stream_Type with
			Stream => Stream,
			Deflator => Deflate_Init (
				Level => Level,
				Method => Method,
				Window_Bits => Window_Bits,
				Header => Header,
				Memory_Level => Memory_Level,
				Strategy => Strategy));
	end Open;
	
	procedure Close (Object : in out Out_Type) is
	begin
		Close (Object.Deflator);
	end Close;
	
	function Is_Open (Object : Out_Type) return Boolean is
	begin
		return Object.Stream /= null;
	end Is_Open;
	
	function Stream (Object : Out_Type)
		return not null access Ada.Streams.Root_Stream_Type'Class is
	begin
		if not Is_Open (Object) then
			raise Status_Error;
		end if;
		return Object'Unrestricted_Access;
	end Stream;
	
	procedure Finish (Object : in out Out_Type) is
	begin
		if not Is_Open (Object) then
			raise Status_Error;
		end if;
		declare
			procedure Deflating_Finish is new Generic_Finish (Deflate);
		begin
			Deflating_Finish (Object.Stream, Object.Deflator);
		end;
	end Finish;
	
	overriding procedure Read (
		Object : in out Out_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset) is
	begin
		raise Program_Error; -- exclusive use for writing
	end Read;
	
	overriding procedure Write (
		Object : in out Out_Type;
		Item : in Ada.Streams.Stream_Element_Array)
	is
		procedure Deflating_Write is new Generic_Write (Deflate);
	begin
		Deflating_Write (Object.Stream, Item, Object.Deflator);
	end Write;
	
	-- only reading with inflation
	
	function Open (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Inflation_Header := Auto;
		Buffer_Length : Ada.Streams.Stream_Element_Count :=
			Default_Buffer_Length)
		return In_Type
	is
		pragma Suppress (Accessibility_Check);
	begin
		return (Ada.Streams.Root_Stream_Type with
			Buffer_Length => Buffer_Length,
			Stream => Stream,
			Inflator => Inflate_Init (
				Window_Bits => Window_Bits,
				Header => Header),
			In_Buffer => <>,
			In_First => 0,
			In_Last => -1);
	end Open;
	
	procedure Close (Object : in out In_Type) is
	begin
		Close (Object.Inflator);
	end Close;
	
	function Is_Open (Object : In_Type) return Boolean is
	begin
		return Object.Stream /= null;
	end Is_Open;
	
	function Stream (Object : In_Type)
		return not null access Ada.Streams.Root_Stream_Type'Class is
	begin
		if not Is_Open (Object) then
			raise Status_Error;
		end if;
		return Object'Unrestricted_Access;
	end Stream;
	
	overriding procedure Read (
		Object : in out In_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset)
	is
		procedure Deflating_Read is new Generic_Read (Inflate);
	begin
		Deflating_Read (
			Object.Stream,
			Item,
			Last,
			Object.Inflator,
			Object.In_First,
			Object.In_Last,
			Object.In_Buffer);
	end Read;
	
	overriding procedure Write (
		Object : in out In_Type;
		Item : in Ada.Streams.Stream_Element_Array) is
	begin
		raise Program_Error; -- exclusive use for reading
	end Write;
	
	-- compatibility
	
	procedure Create (
		Stream : in out Stream_Type'Class;
		Mode : in Stream_Mode;
		Back : in Stream_Access;
		Back_Compressed : in Boolean;
		Level : in Compression_Level := Default_Compression;
		Strategy : in Strategy_Type := Default_Strategy;
		Header : in Header_Type := Default) is
	begin
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
	end Create;
	
	procedure Close (Object : in out Stream_Type'Class) is
	begin
		Close (Object.Raw);
	end Close;
	
	function Is_Open (Object : Stream_Type'Class) return Boolean is
	begin
		return Object.Target /= null;
	end Is_Open;
	
	procedure Flush (
		Stream : in out Stream_Type'Class;
		Mode : in Flush_Mode) is
	begin
		if not Is_Open (Stream) then
			raise Status_Error;
		end if;
		if Mode then
			declare
				procedure DI_Finish is new Generic_Finish (Deflate_Or_Inflate);
			begin
				DI_Finish (Stream.Target, Stream.Raw);
			end;
		end if;
	end Flush;
	
	function Read_Total_In (Stream : Stream_Type'Class) return Count is
	begin
		return Total_In (Stream.Raw);
	end Read_Total_In;
	
	function Read_Total_Out (Stream : Stream_Type'Class) return Count is
	begin
		return Total_Out (Stream.Raw);
	end Read_Total_Out;
	
	function Write_Total_In (Stream : Stream_Type'Class) return Count
		renames Read_Total_Out;
	
	function Write_Total_Out (Stream : Stream_Type'Class) return Count
		renames Read_Total_Out;
	
	overriding procedure Read (
		Object : in out Stream_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset) is
	begin
		if not Is_Open (Object) or else Object.Direction = Out_Stream then
			raise Status_Error;
		end if;
		declare
			procedure DI_Read is new Generic_Read (Deflate_Or_Inflate);
		begin
			DI_Read (
				Object.Target,
				Item,
				Last,
				Object.Raw,
				Object.In_First,
				Object.In_Last,
				Object.In_Buffer);
		end;
	end Read;
	
	overriding procedure Write (
		Object : in out Stream_Type;
		Item : in Ada.Streams.Stream_Element_Array) is
	begin
		if not Is_Open (Object) or else Object.Direction = In_Stream then
			raise Status_Error;
		end if;
		declare
			procedure DI_Write is new Generic_Write (Deflate_Or_Inflate);
		begin
			DI_Write (Object.Target, Item, Object.Raw);
		end;
	end Write;
	
end zlib.Streams;
