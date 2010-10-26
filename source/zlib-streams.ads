package zlib.Streams is
	pragma Preelaborate;
	use type Ada.Streams.Stream_Element_Offset;
	
	type Stream (Buffer_Length : Ada.Streams.Stream_Element_Count) is
		limited new Ada.Streams.Root_Stream_Type with private;
	
	type Direction is (Writing, Reading);
	
	function Create_Deflation (
		Direction : Streams.Direction := Writing;
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Level : Compression_Level := Default_Compression;
		Method : Compression_Method := Deflated;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Deflation_Header := Default;
		Memory_Level : zlib.Memory_Level := Default_Memory_Level;
		Strategy : zlib.Strategy := Default_Strategy)
		return Stream;
	
	function Create_Inflation (
		Direction : Streams.Direction := Reading;
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Inflation_Header := Auto)
		return Stream;
	
	procedure Finish (Object : in out Stream);
	
	-- compatiblity with Zlib.Ada.
	
	type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
	subtype Stream_Type is Stream (Buffer_Length => 2 ** 15);
	subtype Stream_Mode is Direction;
	function Out_Stream return Stream_Mode renames Writing;
	function In_Stream return Stream_Mode renames Reading;
	
	procedure Create (
		Stream : in out Streams.Stream;
		Mode : in Stream_Mode;
		Back : in Stream_Access;
		Back_Compressed : in Boolean;
		Level : in Compression_Level := Default_Compression;
		Strategy : in Strategy_Type := Default_Strategy;
		Header : in Header_Type := Default);
	
	procedure Flush (
		Stream : in out Streams.Stream;
		Mode : in Flush_Mode);
	
	procedure Close (Stream : in out Streams.Stream);
	
	function Read_Total_In (Stream : Streams.Stream) return Count;
	function Read_Total_Out (Stream : Streams.Stream) return Count;
	function Write_Total_In (Stream : Streams.Stream) return Count;
	function Write_Total_Out (Stream : Streams.Stream) return Count;
	
private
	
	type Stream (Buffer_Length : Ada.Streams.Stream_Element_Count) is
		limited new Ada.Streams.Root_Stream_Type with
	record
		Direction : Streams.Direction;
		Target : access Ada.Streams.Root_Stream_Type'Class;
		Raw : zlib.Stream;
		In_Buffer : Ada.Streams.Stream_Element_Array (1 .. Buffer_Length);
		In_First : Ada.Streams.Stream_Element_Offset;
		In_Last : Ada.Streams.Stream_Element_Offset;
	end record;
	
	overriding procedure Read (
		Object : in out Stream;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Stream;
		Item : in Ada.Streams.Stream_Element_Array);
	
end zlib.Streams;
