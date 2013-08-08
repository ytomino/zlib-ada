package zlib.Strings is
	pragma Preelaborate;
	
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
		In_Item : in String;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset);
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean)
		renames zlib.Deflate;
	
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
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String;
		Out_Last : out Natural);
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		Out_Item : out String;
		Out_Last : out Natural;
		Finish : in Boolean;
		Finished : out Boolean);
	
end zlib.Strings;
