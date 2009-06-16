
signature HTTP_PACKETIZER =
sig

  exception Http of string

  datatype http_packet =
    Headers of string list
  | Content of string

  include PACKETIZER where type packet = http_packet

end
