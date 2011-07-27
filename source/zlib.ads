with Ada.Finalization;
with Ada.IO_Exceptions;
with Ada.Streams;
private with C.zconf;
private with C.zlib;
package zlib is
	pragma Preelaborate;
	pragma Linker_Options ("-lz");
	
	function Version return String;
	
	type Stream is limited private;
	
	-- level
	type Compression_Level is range -1 .. 9;
	No_Compression : constant Compression_Level;
	Best_Speed : constant Compression_Level;
	Best_Compression : constant Compression_Level;
	Default_Compression : constant Compression_Level;
	
	-- method
	package Compression_Methods is
		type Compression_Method is (Deflated);
	private
		for Compression_Method use (Deflated => C.zlib.Z_DEFLATED);
	end Compression_Methods;
	type Compression_Method is new Compression_Methods.Compression_Method;
	
	-- windowBits
	type Window_Bits is range 8 .. 15;
	Default_Window_Bits : constant Window_Bits := 15;
	
	type Inflation_Header is (None, Default, GZip, Auto);
	subtype Deflation_Header is Inflation_Header range None .. GZip;
	
	-- memLevel
	type Memory_Level is range 1 .. 9;
	Default_Memory_Level : constant Memory_Level := 8;
	
	-- stragegy
	package Strategies is
		type Strategy is (
			Default_Strategy,
			Filtered,
			Huffman_Only,
			RLE,
			Fixed);
	private
		for Strategy use (
			Default_Strategy => C.zlib.Z_DEFAULT_STRATEGY,
			Filtered => C.zlib.Z_FILTERED,
			Huffman_Only => C.zlib.Z_HUFFMAN_ONLY,
			RLE => C.zlib.Z_RLE,
			Fixed => C.zlib.Z_FIXED);
	end Strategies;
	type Strategy is new Strategies.Strategy;
	
	function Deflate_Init (
		Level : Compression_Level := Default_Compression;
		Method : Compression_Method := Deflated;
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Deflation_Header := Default;
		Memory_Level : zlib.Memory_Level := Default_Memory_Level;
		Strategy : zlib.Strategy := Default_Strategy)
		return Stream;
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean);
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in String;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean);
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset);
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in String;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset);
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean);
	
	function Inflate_Init (
		Window_Bits : zlib.Window_Bits := Default_Window_Bits;
		Header : Inflation_Header := Auto)
		return Stream;
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean);
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String;
		Out_Last : out Natural;
		Finish : in Boolean;
		Finished : out Boolean);
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean);
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		Out_Item : out String;
		Out_Last : out Natural;
		Finish : in Boolean;
		Finished : out Boolean);
	
	function Total_In (Stream : zlib.Stream)
		return Ada.Streams.Stream_Element_Count;
	function Total_Out (Stream : zlib.Stream)
		return Ada.Streams.Stream_Element_Count;
	
	Data_Error : exception renames Ada.IO_Exceptions.Data_Error;
	Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
	
	-- compatibility with ZLib.Ada.
	
	subtype Count is Ada.Streams.Stream_Element_Count;
	
	subtype Filter_Type is Stream;
	subtype Header_Type is Inflation_Header;
	subtype Strategy_Type is Strategy;
	subtype Flush_Mode is Boolean;
	function No_Flush return Boolean renames False;
	function Finish return Boolean renames True;
	
	procedure Deflate_Init (
		Filter : in out Filter_Type;
		Level : in Compression_Level := Default_Compression;
		Method : in Compression_Method := Deflated;
		Window_Bits : in zlib.Window_Bits := Default_Window_Bits;
		Header : in Deflation_Header := Default;
		Memory_Level : in zlib.Memory_Level := Default_Memory_Level;
		Strategy : in Strategy_Type := Default_Strategy);
	
	procedure Inflate_Init (
		Filter : in out Filter_Type;
		Window_Bits : in zlib.Window_Bits := Default_Window_Bits;
		Header : in Header_Type := Auto);
	
	procedure Close (Filter : in out Filter_Type);
	
	procedure Translate (
		Filter : in out Filter_Type;
		In_Data : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Data : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Flush : in Flush_Mode);
	
	generic
		with procedure Data_In (
			Item : out Ada.Streams.Stream_Element_Array;
			Last : out Ada.Streams.Stream_Element_Offset);
		with procedure Data_Out (
			Item : in Ada.Streams.Stream_Element_Array);
	procedure Generic_Translate (Filter : in out Filter_Type);
	
	generic
		with procedure Write (Item : in Ada.Streams.Stream_Element_Array);
	procedure Write (
		Filter : in out Filter_Type;
		Item : in Ada.Streams.Stream_Element_Array;
		Flush : in Flush_Mode := No_Flush);
	
	generic
		with procedure Read (
			Item : out Ada.Streams.Stream_Element_Array;
			Last : out Ada.Streams.Stream_Element_Offset);
		Buffer : in out Ada.Streams.Stream_Element_Array;
		Rest_First, Rest_Last : in out Ada.Streams.Stream_Element_Offset;
	procedure Read (
		Filter : in out Filter_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Flush : in Flush_Mode := No_Flush);
	
private
	use type Ada.Streams.Stream_Element_Offset;
	
	pragma Compile_Time_Error (Window_Bits'Last /= C.zconf.MAX_WBITS,
		"MAX_WBITS is mismatch");
	pragma Compile_Time_Error (Memory_Level'Last /= C.zconf.MAX_MEM_LEVEL,
		"MAX_MEM_LEVEL is mismatch");
	
	type Status is (Closed, Deflating, Inflating);
	pragma Discard_Names (Status);
	
	type Stream is limited new Ada.Finalization.Limited_Controlled with record
		Z_Stream : aliased C.zlib.z_stream;
		Status : zlib.Status := Closed;
		Stream_End : Boolean;
	end record;
	
	overriding procedure Finalize (Object : in out Stream);
	
	No_Compression : constant Compression_Level := C.zlib.Z_NO_COMPRESSION;
	Best_Speed : constant Compression_Level := C.zlib.Z_BEST_SPEED;
	Best_Compression : constant Compression_Level := C.zlib.Z_BEST_COMPRESSION;
	Default_Compression : constant Compression_Level :=
		C.zlib.Z_DEFAULT_COMPRESSION;
	
	procedure Deflate_Or_Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean);
	
end zlib;
