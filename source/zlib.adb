with C.string;
package body zlib is
	use type C.signed_int;
	use type C.size_t;
	
	type unsigned_char_array is
		array (C.size_t range <>) of aliased C.unsigned_char;
	pragma Suppress_Initialization (unsigned_char_array);
	pragma Convention (C, unsigned_char_array);
	-- C.unsigned_char_array is not generated in some cases
	
	Flush_Table : constant array (Boolean) of C.signed_int := (
		C.zlib.Z_NO_FLUSH, C.zlib.Z_FINISH);
	
	function To_String (S : not null access constant C.char) return String is
		Result : String (1 .. Natural (C.string.strlen (S)));
		for Result'Address use S.all'Address;
	begin
		return Result;
	end To_String;
	
	procedure Raise_Error (Result : C.signed_int);
	pragma No_Return (Raise_Error);
	procedure Raise_Error (Result : C.signed_int) is
	begin
		case Result is
			when C.zlib.Z_DATA_ERROR =>
				raise Data_Error with To_String (C.zlib.zError (Result));
			when C.zlib.Z_MEM_ERROR =>
				raise Storage_Error with To_String (C.zlib.zError (Result));
			when C.zlib.Z_BUF_ERROR =>
				raise Constraint_Error with To_String (C.zlib.zError (Result));
			when others =>
				null; -- move to below to suppress the warning, bug(?) of gcc-4.5.1
		end case;
		raise Status_Error with To_String (C.zlib.zError (Result));
	end Raise_Error;
	
	function Make_Window_Bits (
		Window_Bits : zlib.Window_Bits;
		Header : Inflation_Header)
		return C.signed_int is
	begin
		case Header is
			when None =>
				return -C.signed_int (Window_Bits);
			when Default =>
				return C.signed_int (Window_Bits);
			when GZip =>
				return C.signed_int (Window_Bits + 16);
			when Auto =>
				return C.signed_int (Window_Bits + 32);
		end case;
	end Make_Window_Bits;
	
	procedure Internal_Deflate_Init (
		Z_Stream : not null access C.zlib.z_stream;
		Level : in Compression_Level;
		Method : in Compression_Method;
		Window_Bits : in zlib.Window_Bits;
		Header : in Deflation_Header;
		Memory_Level : in zlib.Memory_Level;
		Strategy : in zlib.Strategy)
	is
		Result : constant C.signed_int := C.zlib.deflateInit2q (
			Z_Stream,
			level => Compression_Level'Enum_Rep (Level),
			method => Compression_Method'Enum_Rep (Method),
			windowBits => Make_Window_Bits (Window_Bits, Header),
			memLevel => zlib.Memory_Level'Enum_Rep (Memory_Level),
			strategy => zlib.Strategy'Enum_Rep (Strategy),
			version => C.zlib.ZLIB_VERSION (C.zlib.ZLIB_VERSION'First)'Access,
			stream_size => C.zlib.z_stream'Size / Standard'Storage_Unit);
	begin
		if Result /= C.zlib.Z_OK then
			Raise_Error (Result);
		end if;
	end Internal_Deflate_Init;
	
	procedure Internal_Deflate_End (
		Z_Stream : not null access C.zlib.z_stream)
	is
		Result : constant C.signed_int := C.zlib.deflateEnd (Z_Stream);
	begin
		if Result /= C.zlib.Z_OK then
			Raise_Error (Result);
		end if;
	end Internal_Deflate_End;
	
	procedure Internal_Inflate_Init (
		Z_Stream : not null access C.zlib.z_stream;
		Window_Bits : in zlib.Window_Bits;
		Header : in Inflation_Header)
	is
		Result : constant C.signed_int := C.zlib.inflateInit2q (
			Z_Stream,
			windowBits => Make_Window_Bits (Window_Bits, Header),
			version => C.zlib.ZLIB_VERSION (C.zlib.ZLIB_VERSION'First)'Access,
			stream_size => C.zlib.z_stream'Size / Standard'Storage_Unit);
	begin
		if Result /= C.zlib.Z_OK then
			Raise_Error (Result);
		end if;
	end Internal_Inflate_Init;
	
	procedure Internal_Inflate_End (
		Z_Stream : not null access C.zlib.z_stream)
	is
		Result : constant C.signed_int := C.zlib.inflateEnd (Z_Stream);
	begin
		if Result /= C.zlib.Z_OK then
			Raise_Error (Result);
		end if;
	end Internal_Inflate_End;
	
	-- implementation
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean) is
	begin
		if Status (Stream) /= Deflating then
			raise Status_Error;
		else
			declare
				Z_Stream : constant not null access C.zlib.z_stream := Z (Stream);
				C_In_Size : constant C.size_t := In_Item'Length;
				C_In_Item : unsigned_char_array (0 .. C_In_Size - 1);
				for C_In_Item'Address use In_Item'Address;
				C_Out_Size : constant C.size_t := Out_Item'Length;
				C_Out_Item : unsigned_char_array (0 .. C_Out_Size - 1);
				for C_Out_Item'Address use Out_Item'Address;
				Result : C.signed_int;
			begin
				Z_Stream.next_in := C_In_Item (0)'Unchecked_Access;
				Z_Stream.avail_in := C.zconf.uInt (C_In_Size);
				Z_Stream.next_out := C_Out_Item (0)'Unchecked_Access;
				Z_Stream.avail_out := C.zconf.uInt (C_Out_Size);
				Result := C.zlib.deflate (Z_Stream, Flush_Table (Finish));
				case Result is
					when C.zlib.Z_OK | C.zlib.Z_STREAM_END =>
						In_Last := In_Item'First
							+ Ada.Streams.Stream_Element_Offset (C_In_Size)
							- Ada.Streams.Stream_Element_Offset (Z_Stream.avail_in)
							- 1;
						Out_Last := Out_Item'First
							+ Ada.Streams.Stream_Element_Offset (C_Out_Size)
							- Ada.Streams.Stream_Element_Offset (Z_Stream.avail_out)
							- 1;
						Finished := Result = C.zlib.Z_STREAM_END;
						if Finished then
							Set_End (Stream);
						end if;
					when others =>
						Raise_Error (Result);
				end case;
			end;
		end if;
	end Deflate;
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset)
	is
		Dummy_Finished : Boolean;
	begin
		Deflate (
			Stream,
			In_Item,
			In_Last,
			Out_Item,
			Out_Last,
			False,
			Dummy_Finished);
	end Deflate;
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean)
	is
		Dummy_In_Item : Ada.Streams.Stream_Element_Array (1 .. 0);
		Dummy_In_Last : Ada.Streams.Stream_Element_Offset;
	begin
		Deflate (
			Stream,
			Dummy_In_Item,
			Dummy_In_Last,
			Out_Item,
			Out_Last,
			Finish,
			Finished);
	end Deflate;
	
	function Deflate_Init (
		Level : Compression_Level := Default_Compression;
		Method : Compression_Method := Deflated;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Deflation_Header := Default;
		Memory_Level : zlib.Memory_Level := Default_Memory_Level;
		Strategy : zlib.Strategy := Default_Strategy)
		return Stream is
	begin
		return Result : Stream do
			Set_Start (Result, Deflating);
			Internal_Deflate_Init (
				Z (Result),
				Level,
				Method,
				Window_Bits,
				Header,
				Memory_Level,
				Strategy);
		end return;
	end Deflate_Init;
	
	procedure Deflate_Or_Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean) is
	begin
		case Status (Stream) is
			when Closed =>
				raise Status_Error;
			when Deflating =>
				Deflate (
					Stream,
					In_Item,
					In_Last,
					Out_Item,
					Out_Last,
					Finish,
					Finished);
			when Inflating =>
				Inflate (
					Stream,
					In_Item,
					In_Last,
					Out_Item,
					Out_Last,
					Finish,
					Finished);
		end case;
	end Deflate_Or_Inflate;
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean) is
	begin
		if Status (Stream) /= Inflating then
			raise Status_Error;
		else
			declare
				Z_Stream : constant not null access C.zlib.z_stream := Z (Stream);
				C_In_Size : constant C.size_t := In_Item'Length;
				C_In_Item : unsigned_char_array (0 .. C_In_Size - 1);
				for C_In_Item'Address use In_Item'Address;
				C_Out_Size : constant C.size_t := Out_Item'Length;
				C_Out_Item : unsigned_char_array (0 .. C_Out_Size - 1);
				for C_Out_Item'Address use Out_Item'Address;
				Result : C.signed_int;
			begin
				Z_Stream.next_in := C_In_Item (0)'Unchecked_Access;
				Z_Stream.avail_in := C.zconf.uInt (C_In_Size);
				Z_Stream.next_out := C_Out_Item (0)'Unchecked_Access;
				Z_Stream.avail_out := C.zconf.uInt (C_Out_Size);
				Result := C.zlib.inflate (Z_Stream, Flush_Table (Finish));
				case Result is
					when C.zlib.Z_OK | C.zlib.Z_STREAM_END =>
						In_Last := In_Item'First
							+ Ada.Streams.Stream_Element_Offset (C_In_Size)
							- Ada.Streams.Stream_Element_Offset (Z_Stream.avail_in)
							- 1;
						Out_Last := Out_Item'First
							+ Ada.Streams.Stream_Element_Offset (C_Out_Size)
							- Ada.Streams.Stream_Element_Offset (Z_Stream.avail_out)
							- 1;
						Finished := Result = C.zlib.Z_STREAM_END;
						if Finished then
							Set_End (Stream);
						end if;
					when others =>
						Raise_Error (Result);
				end case;
			end;
		end if;
	end Inflate;
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean)
	is
		Dummy_In_Item : Ada.Streams.Stream_Element_Array (1 .. 0);
		Dummy_In_Last : Ada.Streams.Stream_Element_Offset;
	begin
		Inflate (
			Stream,
			Dummy_In_Item,
			Dummy_In_Last,
			Out_Item,
			Out_Last,
			Finish,
			Finished);
	end Inflate;
	
	function Inflate_Init (
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Inflation_Header := Auto)
		return Stream is
	begin
		return Result : Stream do
			Set_Start (Result, Inflating);
			Internal_Inflate_Init (
				Z (Result),
				Window_Bits,
				Header);
		end return;
	end Inflate_Init;
	
	function Total_In (Stream : zlib.Stream)
		return Ada.Streams.Stream_Element_Count is
	begin
		return Ada.Streams.Stream_Element_Count (Z (Stream).total_in);
	end Total_In;
	
	function Total_Out (Stream : zlib.Stream)
		return Ada.Streams.Stream_Element_Count is
	begin
		return Ada.Streams.Stream_Element_Count (Z (Stream).total_out);
	end Total_Out;
	
	function Version return String is
	begin
		return To_String (C.zlib.zlibVersion);
	end Version;
	
	package body Primitives is
		
		function Z (Object : Stream) return not null access C.zlib.z_stream is
		begin
			return Object.Z_Stream'Unrestricted_Access;
			-- Object parameter shold be "aliased" in Ada 2012
		end Z;
		
		function Status (Object : Stream) return Status_Type is
		begin
			return Object.Status;
		end Status;
		
		function End_Of (Object : Stream) return Boolean is
		begin
			return Object.Stream_End;
		end End_Of;
		
		procedure Set_Start (Object : in out Stream; Status : in Status_Type) is
		begin
			Object.Status := Status;
			Object.Stream_End := False;
		end Set_Start;
		
		procedure Set_End (Object : in out Stream) is
		begin
			Object.Stream_End := True;
		end Set_End;
		
		overriding procedure Finalize (Object : in out Stream) is
		begin
			case Object.Status is
				when Closed =>
					null;
				when Deflating =>
					Internal_Deflate_End (Object.Z_Stream'Access);
					Object.Status := Closed;
				when Inflating =>
					Internal_Inflate_End (Object.Z_Stream'Access);
					Object.Status := Closed;
			end case;
		exception
			when others => null;
		end Finalize;
		
	end Primitives;
	
	-- compatibility
	
	procedure Close (Filter : in out Filter_Type) renames Finalize;
	
	procedure Deflate_Init (
		Filter : in out Filter_Type;
		Level : in Compression_Level := Default_Compression;
		Method : in Compression_Method := Deflated;
		Window_Bits : in zlib.Window_Bits := Default_Window_Bits;
		Header : in Deflation_Header := Default;
		Memory_Level : in zlib.Memory_Level := Default_Memory_Level;
		Strategy : in Strategy_Type := Default_Strategy) is
	begin
		if Status (Filter) /= Closed then
			raise Status_Error;
		else
			Set_Start (Filter, Deflating);
			Internal_Deflate_Init (
				Z (Filter),
				Level,
				Method,
				Window_Bits,
				Header,
				Memory_Level,
				Strategy);
		end if;
	end Deflate_Init;
	
	procedure Generic_Translate (Filter : in out Filter_Type) is
		In_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
		In_First : Ada.Streams.Stream_Element_Offset := In_Item'Last + 1;
		In_Last : Ada.Streams.Stream_Element_Offset;
		In_Used : Ada.Streams.Stream_Element_Offset;
		Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
		Out_Last : Ada.Streams.Stream_Element_Offset;
	begin
		loop
			if In_First > In_Item'Last then
				Data_In (In_Item, In_Last);
				In_First := In_Item'First;
			end if;
			Translate (
				Filter,
				In_Item (In_First .. In_Last),
				In_Used,
				Out_Item,
				Out_Last,
				In_Last < In_Item'Last);
			Data_Out (Out_Item (Out_Item'First .. Out_Last));
			exit when End_Of (Filter);
			In_First := In_Used + 1;
		end loop;
	end Generic_Translate;
	
	procedure Inflate_Init (
		Filter : in out Filter_Type;
		Window_Bits : in zlib.Window_Bits := Default_Window_Bits;
		Header : in Header_Type := Auto) is
	begin
		if Status (Filter) /= Closed then
			raise Status_Error;
		else
			Set_Start (Filter, Inflating);
			Internal_Inflate_Init (
				Z (Filter),
				Window_Bits,
				Header);
		end if;
	end Inflate_Init;
	
	procedure Read (
		Filter : in out Filter_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Flush : in Flush_Mode := No_Flush) is
	begin
		if End_Of (Filter) then
			Last := Item'First - 1;
		else
			declare
				In_Used : Ada.Streams.Stream_Element_Offset;
				Out_First : Ada.Streams.Stream_Element_Offset := Item'First;
				Finish : Boolean := Flush;
			begin
				loop
					if Rest_First > Rest_Last then
						Read (Buffer, Rest_Last);
						Rest_First := Buffer'First;
						if Status (Filter) = Deflating then
							Finish := Finish or else Rest_Last < Buffer'Last;
						end if;
					end if;
					Translate (
						Filter,
						Buffer (Rest_First .. Rest_Last),
						In_Used,
						Item (Out_First .. Item'Last),
						Last,
						Finish);
					Rest_First := In_Used + 1;
					exit when End_Of (Filter) or else Last >= Item'Last;
					Out_First := Last + 1;
				end loop;
			end;
		end if;
	end Read;
	
	procedure Translate (
		Filter : in out Filter_Type;
		In_Data : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Data : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Flush : in Flush_Mode)
	is
		Dummy_Finished : Boolean;
	begin
		Deflate_Or_Inflate (
			Filter,
			In_Data,
			In_Last,
			Out_Data,
			Out_Last,
			Flush,
			Dummy_Finished);
	end Translate;
	
	procedure Write (
		Filter : in out Filter_Type;
		Item : in Ada.Streams.Stream_Element_Array;
		Flush : in Flush_Mode := No_Flush) is
	begin
		if Flush or Item'First <= Item'Last then
			declare
				In_First : Ada.Streams.Stream_Element_Offset := Item'First;
				In_Used : Ada.Streams.Stream_Element_Offset;
				Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
				Out_Last : Ada.Streams.Stream_Element_Offset;
			begin
				loop
					Translate (
						Filter,
						Item (In_First .. Item'Last),
						In_Used,
						Out_Item,
						Out_Last,
						Flush);
					Write (Out_Item (Out_Item'First .. Out_Last));
					exit when End_Of (Filter) or else In_Used >= Item'Last;
					In_First := In_Used + 1;
				end loop;
			end;
		end if;
	end Write;
	
end zlib;
