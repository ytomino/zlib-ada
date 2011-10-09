with Ada.Text_IO;
with zlib;
procedure version is
begin
	Ada.Text_IO.Put_Line (zlib.Version);
end version;
