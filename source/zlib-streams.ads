with Ada.Streams.Stream_IO;
private with System;
package zlib.Streams is
	pragma Preelaborate;
	use type Ada.Streams.Stream_Element_Offset;
	
	Default_Buffer_Length : constant := 4096; -- same as Zlib.Ada.
	
	-- only writing with deflation
	
	type Out_Type is
		limited private;
	
--	subtype Open_Out_Type is Out_Type
--		with
--			Dynamic_Predicate => Is_Open (Open_Out_Type),
--			Predicate_Failure => raise Status_Error;
	
	function Open (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Level : Compression_Level := Default_Compression;
		Method : Compression_Method := Deflated;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Deflation_Header := Default;
		Memory_Level : zlib.Memory_Level := Default_Memory_Level;
		Strategy : zlib.Strategy := Default_Strategy)
		return Out_Type;
	
	procedure Close (Object : in out Out_Type);
	
	function Is_Open (Object : Out_Type) return Boolean;
	
	function Stream (
		Object : in out Out_Type) -- Open_Out_Type
		return not null access Ada.Streams.Root_Stream_Type'Class;
	
	procedure Finish (
		Object : in out Out_Type); -- Open_Out_Type
	
	-- only reading with inflation
	
	type In_Type (Buffer_Length : Ada.Streams.Stream_Element_Count) is
		limited private;
	
--	subtype Open_In_Type is In_Type
--		with
--			Dynamic_Predicate => Is_Open (Open_In_Type),
--			Predicate_Failure => raise Status_Error;
	
	function Open (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Inflation_Header := Auto;
		Buffer_Length : Ada.Streams.Stream_Element_Count :=
			Default_Buffer_Length)
		return In_Type;
	
	procedure Close (Object : in out In_Type);
	
	function Is_Open (Object : In_Type) return Boolean;
	
	function Stream (
		Object : in out In_Type) -- Open_In_Type
		return not null access Ada.Streams.Root_Stream_Type'Class;
	
	-- compatiblity with Zlib.Ada.
	
	subtype Stream_Access is Ada.Streams.Stream_IO.Stream_Access;
	type Stream_Type is limited new Ada.Streams.Root_Stream_Type with private;
	type Stream_Mode is (In_Stream, Out_Stream);
	
	procedure Create (
		Stream : in out Stream_Type'Class;
		Mode : in Stream_Mode;
		Back : access Ada.Streams.Root_Stream_Type'Class;
		Back_Compressed : in Boolean;
		Level : in Compression_Level := Default_Compression;
		Strategy : in Strategy_Type := Default_Strategy;
		Header : in Header_Type := Default);
	
	procedure Close (Object : in out Stream_Type'Class);
	
	function Is_Open (Object : Stream_Type'Class) return Boolean;
	
	procedure Flush (
		Stream : in out Stream_Type'Class;
		Mode : in Flush_Mode);
	
	function Read_Total_In (Stream : Stream_Type'Class) return Count;
	function Read_Total_Out (Stream : Stream_Type'Class) return Count;
	function Write_Total_In (Stream : Stream_Type'Class) return Count;
	function Write_Total_Out (Stream : Stream_Type'Class) return Count;
	
	-- exceptions
	
	End_Error : exception
		renames Ada.IO_Exceptions.End_Error;
	
private
	
	-- only writing with deflation
	
	type Out_Type is
		limited new Ada.Streams.Root_Stream_Type with
	record
		Stream : System.Address; -- access Ada.Streams.Root_Stream_Type'Class;
		Deflator : zlib.Stream;
	end record;
	
	overriding procedure Read (
		Object : in out Out_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Out_Type;
		Item : in Ada.Streams.Stream_Element_Array);
	
	-- only reading with inflation
	
	type In_Type (Buffer_Length : Ada.Streams.Stream_Element_Count) is
		limited new Ada.Streams.Root_Stream_Type with
	record
		Stream : System.Address; -- access Ada.Streams.Root_Stream_Type'Class;
		Inflator : zlib.Stream;
		In_First : Ada.Streams.Stream_Element_Offset;
		In_Last : Ada.Streams.Stream_Element_Offset;
		In_Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Length);
	end record;
	
	overriding procedure Read (
		Object : in out In_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out In_Type;
		Item : in Ada.Streams.Stream_Element_Array);
	
	-- compatiblity with Zlib.Ada.
	
	type Stream_Type is
		limited new Ada.Streams.Root_Stream_Type with
	record
		Direction : Stream_Mode;
		Target : System.Address; -- access Ada.Streams.Root_Stream_Type'Class;
		Raw : zlib.Stream;
		In_Buffer : Ada.Streams.Stream_Element_Array (
			1 ..
			Default_Buffer_Length);
		In_First : Ada.Streams.Stream_Element_Offset;
		In_Last : Ada.Streams.Stream_Element_Offset;
	end record;
	
	overriding procedure Read (
		Object : in out Stream_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Stream_Type;
		Item : in Ada.Streams.Stream_Element_Array);
	
end zlib.Streams;
