with System;
with C.string;
package body zlib is
	use type C.signed_int;
	use type C.size_t;
	
	type unsigned_char_array is
		array (C.size_t range <>) of aliased C.unsigned_char
		with Convention => C;
	pragma Suppress_Initialization (unsigned_char_array);
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
				raise Use_Error with To_String (C.zlib.zError (Result));
		end case;
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
		Stream : in out zlib.Stream;
		Level : in Compression_Level;
		Method : in Compression_Method;
		Window_Bits : in zlib.Window_Bits;
		Header : in Deflation_Header;
		Memory_Level : in zlib.Memory_Level;
		Strategy : in zlib.Strategy)
	is
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Reference (Stream).all;
		Result : C.signed_int;
	begin
		NC_Stream.Z_Stream.zalloc := null;
		NC_Stream.Z_Stream.zfree := null;
		NC_Stream.Z_Stream.opaque := C.void_ptr (System.Null_Address);
		Result := C.zlib.deflateInit2q (
			NC_Stream.Z_Stream'Access,
			level => Compression_Level'Enum_Rep (Level),
			method => Compression_Method'Enum_Rep (Method),
			windowBits => Make_Window_Bits (Window_Bits, Header),
			memLevel => zlib.Memory_Level'Enum_Rep (Memory_Level),
			strategy => zlib.Strategy'Enum_Rep (Strategy),
			version => C.zlib.ZLIB_VERSION (C.zlib.ZLIB_VERSION'First)'Access,
			stream_size => C.zlib.z_stream'Size / Standard'Storage_Unit);
		if Result /= C.zlib.Z_OK then
			Raise_Error (Result);
		end if;
		NC_Stream.Finalize := C.zlib.deflateEnd'Access;
		NC_Stream.Is_Open := True;
		NC_Stream.Mode := Deflating;
		NC_Stream.Stream_End := False;
	end Internal_Deflate_Init;
	
	procedure Internal_Inflate_Init (
		Stream : in out zlib.Stream;
		Window_Bits : in zlib.Window_Bits;
		Header : in Inflation_Header)
	is
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Reference (Stream).all;
		Result : C.signed_int;
	begin
		NC_Stream.Z_Stream.zalloc := null;
		NC_Stream.Z_Stream.zfree := null;
		NC_Stream.Z_Stream.opaque := C.void_ptr (System.Null_Address);
		Result := C.zlib.inflateInit2q (
			NC_Stream.Z_Stream'Access,
			windowBits => Make_Window_Bits (Window_Bits, Header),
			version => C.zlib.ZLIB_VERSION (C.zlib.ZLIB_VERSION'First)'Access,
			stream_size => C.zlib.z_stream'Size / Standard'Storage_Unit);
		if Result /= C.zlib.Z_OK then
			Raise_Error (Result);
		end if;
		NC_Stream.Finalize := C.zlib.inflateEnd'Access;
		NC_Stream.Is_Open := True;
		NC_Stream.Mode := Inflating;
		NC_Stream.Stream_End := False;
	end Internal_Inflate_Init;
	
	procedure Close (
		NC_Stream : in out Non_Controlled_Stream;
		Raise_On_Error : Boolean)
	is
		Result : C.signed_int;
	begin
		Result := NC_Stream.Finalize (NC_Stream.Z_Stream'Access);
		if Result /= C.zlib.Z_OK and then Raise_On_Error then
			Raise_Error (Result);
		end if;
		NC_Stream.Is_Open := False;
	end Close;
	
	-- implementation
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean)
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		pragma Check (Dynamic_Predicate,
			Mode (Stream) = Deflating or else raise Mode_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Reference (Stream).all;
		Z_Stream : constant not null access C.zlib.z_stream :=
			NC_Stream.Z_Stream'Access;
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
					NC_Stream.Stream_End := True;
				end if;
			when others =>
				Raise_Error (Result);
		end case;
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
			Stream, -- Status_Error would be raised if Stream is not open
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
			Stream, -- Status_Error would be raised if Stream is not open
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
			Internal_Deflate_Init (
				Result,
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
		Finished : out Boolean)
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Reference (Stream).all;
	begin
		case NC_Stream.Mode is
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
		Finished : out Boolean)
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		pragma Check (Dynamic_Predicate,
			Mode (Stream) = Inflating or else raise Mode_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Reference (Stream).all;
		Z_Stream : constant not null access C.zlib.z_stream :=
			NC_Stream.Z_Stream'Access;
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
					NC_Stream.Stream_End := True;
				end if;
			when others =>
				Raise_Error (Result);
		end case;
	end Inflate;
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset)
	is
		Dummy_Finished : Boolean;
	begin
		Inflate (
			Stream, -- Status_Error would be raised if Stream is not open
			In_Item,
			In_Last,
			Out_Item,
			Out_Last,
			False,
			Dummy_Finished);
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
			Stream, -- Status_Error would be raised if Stream is not open
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
			Internal_Inflate_Init (
				Result,
				Window_Bits,
				Header);
		end return;
	end Inflate_Init;
	
	procedure Close (Stream : in out zlib.Stream) is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Reference (Stream).all;
	begin
		Close (NC_Stream, Raise_On_Error => True);
	end Close;
	
	function Is_Open (Stream : zlib.Stream) return Boolean is
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Constant_Reference (Stream).all;
	begin
		return NC_Stream.Is_Open;
	end Is_Open;
	
	function Mode (
		Stream : zlib.Stream)
		return Stream_Mode
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Constant_Reference (Stream).all;
	begin
		return NC_Stream.Mode;
	end Mode;
	
	function Total_In (
		Stream : zlib.Stream)
		return Ada.Streams.Stream_Element_Count
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Constant_Reference (Stream).all;
	begin
		return Ada.Streams.Stream_Element_Count (NC_Stream.Z_Stream.total_in);
	end Total_In;
	
	function Total_Out (
		Stream : zlib.Stream)
		return Ada.Streams.Stream_Element_Count
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Stream) or else raise Status_Error);
		NC_Stream : Non_Controlled_Stream
			renames Controlled.Constant_Reference (Stream).all;
	begin
		return Ada.Streams.Stream_Element_Count (NC_Stream.Z_Stream.total_out);
	end Total_Out;
	
	function Version return String is
	begin
		return To_String (C.zlib.zlibVersion);
	end Version;
	
	package body Controlled is
		
		function Constant_Reference (Object : zlib.Stream)
			return not null access constant Non_Controlled_Stream is
		begin
			return Stream (Object).Data'Unchecked_Access;
		end Constant_Reference;
		
		function Reference (Object : in out zlib.Stream)
			return not null access Non_Controlled_Stream is
		begin
			return Stream (Object).Data'Unrestricted_Access;
		end Reference;
		
		overriding procedure Finalize (Object : in out Stream) is
		begin
			if Object.Data.Is_Open then
				Close (Object.Data, Raise_On_Error => False);
			end if;
		end Finalize;
		
	end Controlled;
	
	-- compatibility
	
	procedure Deflate_Init (
		Filter : in out Filter_Type;
		Level : in Compression_Level := Default_Compression;
		Method : in Compression_Method := Deflated;
		Window_Bits : in zlib.Window_Bits := Default_Window_Bits;
		Header : in Deflation_Header := Default;
		Memory_Level : in zlib.Memory_Level := Default_Memory_Level;
		Strategy : in Strategy_Type := Default_Strategy)
	is
		pragma Check (Dynamic_Predicate,
			not Is_Open (Filter) or else raise Status_Error);
	begin
		Internal_Deflate_Init (
			Filter,
			Level,
			Method,
			Window_Bits,
			Header,
			Memory_Level,
			Strategy);
	end Deflate_Init;
	
	procedure Generic_Translate (Filter : in out Filter_Type) is
		NC_Filter : Non_Controlled_Stream
			renames Controlled.Reference (Filter).all;
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
				Filter, -- Status_Error would be raised if Filter is not open
				In_Item (In_First .. In_Last),
				In_Used,
				Out_Item,
				Out_Last,
				In_Last < In_Item'Last);
			Data_Out (Out_Item (Out_Item'First .. Out_Last));
			exit when NC_Filter.Stream_End;
			In_First := In_Used + 1;
		end loop;
	end Generic_Translate;
	
	procedure Inflate_Init (
		Filter : in out Filter_Type;
		Window_Bits : in zlib.Window_Bits := Default_Window_Bits;
		Header : in Header_Type := Auto)
	is
		pragma Check (Dynamic_Predicate,
			not Is_Open (Filter) or else raise Status_Error);
	begin
		Internal_Inflate_Init (
			Filter,
			Window_Bits,
			Header);
	end Inflate_Init;
	
	procedure Read (
		Filter : in out Filter_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Flush : in Flush_Mode := No_Flush)
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Filter) or else raise Status_Error);
		NC_Filter : Non_Controlled_Stream
			renames Controlled.Reference (Filter).all;
	begin
		if NC_Filter.Stream_End then
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
						if NC_Filter.Mode = Deflating then
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
					exit when NC_Filter.Stream_End or else Last >= Item'Last;
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
			Filter, -- Status_Error would be raised if Filter is not open
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
			begin
				loop
					declare
						In_Used : Ada.Streams.Stream_Element_Offset;
						Out_Item : Ada.Streams.Stream_Element_Array (1 .. 2 ** 15);
						Out_Last : Ada.Streams.Stream_Element_Offset;
						Finished : Boolean;
					begin
						Deflate_Or_Inflate (
							Filter, -- Status_Error would be raised if it is not open
							Item (In_First .. Item'Last),
							In_Used,
							Out_Item,
							Out_Last,
							Flush,
							Finished);
						Write (Out_Item (Out_Item'First .. Out_Last));
						exit when Finished or else In_Used >= Item'Last;
						In_First := In_Used + 1;
					end;
				end loop;
			end;
		end if;
	end Write;
	
end zlib;
