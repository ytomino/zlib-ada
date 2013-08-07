package body zlib.Strings is
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in String;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Finished : out Boolean)
	is
		SEA_In_Item : Ada.Streams.Stream_Element_Array (
			Ada.Streams.Stream_Element_Offset (In_Item'First) ..
			Ada.Streams.Stream_Element_Offset (In_Item'Last));
		for SEA_In_Item'Address use In_Item'Address;
		SEA_In_Last : Ada.Streams.Stream_Element_Offset;
	begin
		Deflate (
			Stream,
			SEA_In_Item,
			SEA_In_Last,
			Out_Item,
			Out_Last,
			Finish,
			Finished);
		In_Last := Natural (SEA_In_Last);
	end Deflate;
	
	procedure Deflate (
		Stream : in out zlib.Stream;
		In_Item : in String;
		In_Last : out Natural;
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
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String;
		Out_Last : out Natural;
		Finish : in Boolean;
		Finished : out Boolean)
	is
		SEA_Out_Item : Ada.Streams.Stream_Element_Array (
			Ada.Streams.Stream_Element_Offset (Out_Item'First) ..
			Ada.Streams.Stream_Element_Offset (Out_Item'Last));
		for SEA_Out_Item'Address use Out_Item'Address;
		SEA_Out_Last : Ada.Streams.Stream_Element_Offset;
	begin
		Inflate (
			Stream,
			In_Item,
			In_Last,
			SEA_Out_Item,
			SEA_Out_Last,
			Finish,
			Finished);
		Out_Last := Natural (SEA_Out_Last);
	end Inflate;
	
	procedure Inflate (
		Stream : in out zlib.Stream;
		Out_Item : out String;
		Out_Last : out Natural;
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
	
end zlib.Strings;
