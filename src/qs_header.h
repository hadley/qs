/* qs - Quick Serialization of R Objects
 Copyright (C) 2019-prsent Travers Ching

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can contact the author at:
https://github.com/traversc/qs
*/


#include <Rcpp.h>
#include <fstream>
#include <cstring>
#include <iostream>
#include <algorithm>
#include <memory>
#include <array>
#include <cstddef>
#include <type_traits>
#include <utility>
#include <string>
#include <vector>

#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

#include "RApiSerializeAPI.h"
#include "zstd.h"

using namespace Rcpp;

////////////////////////////////////////////////////////////////
// alt rep string class
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// common utility functions and constants
////////////////////////////////////////////////////////////////

static bool use_alt_rep_bool = true;

bool is_big_endian();

// #define BLOCKSIZE 262144 // 2^20 bytes per block
#define BLOCKRESERVE 64
#define NA_STRING_LENGTH 4294967295 // 2^32-1 -- length used to signify NA value

static uint64_t BLOCKSIZE = 524288;

static const unsigned char list_header_5 = 0x20; 
static const unsigned char list_header_8 = 0x01;
static const unsigned char list_header_16 = 0x02;
static const unsigned char list_header_32 = 0x03;
static const unsigned char list_header_64 = 0x04;

static const unsigned char numeric_header_5 = 0x40; 
static const unsigned char numeric_header_8 = 0x05;
static const unsigned char numeric_header_16 = 0x06;
static const unsigned char numeric_header_32 = 0x07;
static const unsigned char numeric_header_64 = 0x08;

static const unsigned char integer_header_5 = 0x60; 
static const unsigned char integer_header_8 = 0x09;
static const unsigned char integer_header_16 = 0x0A;
static const unsigned char integer_header_32 = 0x0B;
static const unsigned char integer_header_64 = 0x0C;

static const unsigned char logical_header_5 = 0x80; 
static const unsigned char logical_header_8 = 0x0D;
static const unsigned char logical_header_16 = 0x0E;
static const unsigned char logical_header_32 = 0x0F;
static const unsigned char logical_header_64 = 0x10;

static const unsigned char raw_header_32 = 0x17;
static const unsigned char raw_header_64 = 0x18;

static const unsigned char null_header = 0x00; 

static const unsigned char character_header_5 = 0xA0; 
static const unsigned char character_header_8 = 0x11;
static const unsigned char character_header_16 = 0x12;
static const unsigned char character_header_32 = 0x13;
static const unsigned char character_header_64 = 0x14;

static const unsigned char string_header_NA = 0x0F;
static const unsigned char string_header_5 = 0x20; 
static const unsigned char string_header_8 = 0x01;
static const unsigned char string_header_16 = 0x02;
static const unsigned char string_header_32 = 0x03;

static const unsigned char string_enc_native = 0x00; 
static const unsigned char string_enc_utf8 = 0x40;
static const unsigned char string_enc_latin1 = 0x80;
static const unsigned char string_enc_bytes = 0xC0;

static const unsigned char attribute_header_5 = 0xE0;
static const unsigned char attribute_header_8 = 0x1E;
static const unsigned char attribute_header_32 = 0x1F;

static const unsigned char complex_header_32 = 0x15;
static const unsigned char complex_header_64 = 0x16;

static const unsigned char nstype_header_32 = 0x19;
static const unsigned char nstype_header_64 = 0x1A;

static const std::set<SEXPTYPE> stypes = {REALSXP, INTSXP, LGLSXP, STRSXP, CHARSXP, NILSXP, VECSXP, CPLXSXP, RAWSXP};

inline void writeSizeToFile8(std::ofstream & myFile, uint64_t x) {uint64_t x_temp = static_cast<uint64_t>(x); myFile.write(reinterpret_cast<char*>(&x_temp),8);}
inline void writeSizeToFile4(std::ofstream & myFile, uint64_t x) {uint32_t x_temp = static_cast<uint32_t>(x); myFile.write(reinterpret_cast<char*>(&x_temp),4);}
uint64_t readSizeFromFile4(std::ifstream & myFile) {
  std::array<char,4> a = {0,0,0,0};
  myFile.read(a.data(),4);
  return *reinterpret_cast<uint32_t*>(a.data());
}
uint64_t readSizeFromFile8(std::ifstream & myFile) {
  std::array<char,8> a = {0,0,0,0,0,0,0,0};
  myFile.read(a.data(),8);
  return *reinterpret_cast<uint64_t*>(a.data());
}

// casts to <POD> and increments offset
template<typename POD>
inline POD unaligned_cast(char* data, uint64_t offset) {
  POD y;
  std::memcpy(&y, data + offset, sizeof(y));
  return y;
}

// uint32_t char2Size4(std::array<char,4> a) { return *reinterpret_cast<uint32_t*>(a.data()); }
// uint64_t char2Size8(std::array<char,8> a) { return *reinterpret_cast<uint64_t*>(a.data()); }

////////////////////////////////////////////////////////////////
// de-serialization functions
////////////////////////////////////////////////////////////////

struct Data_Context {
  std::ifstream & myFile;
  uint64_t number_of_blocks;
  std::vector<char> zblock;
  std::vector<char> block;
  uint64_t data_offset;
  uint64_t block_i;
  uint64_t block_size;
  std::string temp_string;
  Data_Context(std::ifstream & mf) : myFile(mf) {
    std::array<char,4> reserve_bits = {0,0,0,0};
    myFile.read(reserve_bits.data(),4);
    bool sys_endian = is_big_endian() ? 1 : 0;
    if(reserve_bits[3] != sys_endian) throw exception("Endian of system doesn't match file endian");
    number_of_blocks = readSizeFromFile8(myFile);
    zblock = std::vector<char>(ZSTD_compressBound(BLOCKSIZE));
    block = std::vector<char>(BLOCKSIZE);
    data_offset = 0;
    block_i = 0;
    block_size = 0;
    temp_string = std::string(256, '\0');
  }
  void readHeader(SEXPTYPE & object_type, uint64_t & r_array_len) {
    if(data_offset >= block_size) decompress_block();
    char* header = block.data();
    unsigned char h5 = reinterpret_cast<unsigned char*>(header)[data_offset] & 0xE0;
    switch(h5) {
    case numeric_header_5:
      r_array_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      object_type = REALSXP;
      return;
    case list_header_5:
      r_array_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      object_type = VECSXP;
      return;
    case integer_header_5:
      r_array_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      object_type = INTSXP;
      return;
    case logical_header_5:
      r_array_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      object_type = LGLSXP;
      return;
    case character_header_5:
      r_array_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      object_type = STRSXP;
      return;
    case attribute_header_5:
      r_array_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      object_type = ANYSXP;
      return;
    }
    unsigned char hd = reinterpret_cast<unsigned char*>(header)[data_offset];
    switch(hd) {
    case numeric_header_8:
      r_array_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
      data_offset += 2;
      object_type = REALSXP;
      return;
    case numeric_header_16:
      r_array_len = unaligned_cast<uint16_t>(header, data_offset+1) ;
      data_offset += 3;
      object_type = REALSXP;
      return;
    case numeric_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = REALSXP;
      return;
    case numeric_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = REALSXP;
      return;
    case list_header_8:
      r_array_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
      data_offset += 2;
      object_type = VECSXP;
      return;
    case list_header_16:
      r_array_len = unaligned_cast<uint16_t>(header, data_offset+1) ;
      data_offset += 3;
      object_type = VECSXP;
      return;
    case list_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = VECSXP;
      return;
    case list_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = VECSXP;
      return;
    case integer_header_8:
      r_array_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
      data_offset += 2;
      object_type = INTSXP;
      return;
    case integer_header_16:
      r_array_len = unaligned_cast<uint16_t>(header, data_offset+1) ;
      data_offset += 3;
      object_type = INTSXP;
      return;
    case integer_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = INTSXP;
      return;
    case integer_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = INTSXP;
      return;
    case logical_header_8:
      r_array_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
      data_offset += 2;
      object_type = LGLSXP;
      return;
    case logical_header_16:
      r_array_len = unaligned_cast<uint16_t>(header, data_offset+1) ;
      data_offset += 3;
      object_type = LGLSXP;
      return;
    case logical_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = LGLSXP;
      return;
    case logical_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = LGLSXP;
      return;
    case raw_header_32:
      r_array_len = unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = RAWSXP;
      return;
    case raw_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = RAWSXP;
      return;
    case character_header_8:
      r_array_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
      data_offset += 2;
      object_type = STRSXP;
      return;
    case character_header_16:
      r_array_len = unaligned_cast<uint16_t>(header, data_offset+1) ;
      data_offset += 3;
      object_type = STRSXP;
      return;
    case character_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = STRSXP;
      return;
    case character_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = STRSXP;
      return;
    case complex_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = CPLXSXP;
      return;
    case complex_header_64:
      r_array_len =  unaligned_cast<uint64_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = CPLXSXP;
      return;
    case null_header:
      r_array_len =  0;
      data_offset += 1;
      object_type = NILSXP;
      return;
    case attribute_header_8:
      r_array_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
      data_offset += 2;
      object_type = ANYSXP;
      return;
    case attribute_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = ANYSXP;
      return;
    case nstype_header_32:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 5;
      object_type = S4SXP;
      return;
    case nstype_header_64:
      r_array_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
      data_offset += 9;
      object_type = S4SXP;
      return;
    }
    // additional types
    throw exception("something went wrong (reading object header)");
  }
  void readStringHeader(uint32_t & r_string_len, cetype_t & ce_enc) {
    if(data_offset >= block_size) decompress_block();
    char* header = block.data();
    unsigned char enc = reinterpret_cast<unsigned char*>(header)[data_offset] & 0xC0;
    switch(enc) {
    case string_enc_native:
      ce_enc = CE_NATIVE; break;
    case string_enc_utf8:
      ce_enc = CE_UTF8; break;
    case string_enc_latin1:
      ce_enc = CE_LATIN1; break;
    case string_enc_bytes:
      ce_enc = CE_BYTES; break;
    }
    
    if((reinterpret_cast<unsigned char*>(header)[data_offset] & 0x20) == string_header_5) {
      r_string_len = *reinterpret_cast<uint8_t*>(header+data_offset) & 0x1F ;
      data_offset += 1;
      return;
    } else {
      unsigned char hd = reinterpret_cast<unsigned char*>(header)[data_offset] & 0x1F;
      switch(hd) {
      case string_header_8:
        r_string_len =  *reinterpret_cast<uint8_t*>(header+data_offset+1) ;
        data_offset += 2;
        return;
      case string_header_16:
        r_string_len = unaligned_cast<uint16_t>(header, data_offset+1) ;
        data_offset += 3;
        return;
      case string_header_32:
        r_string_len =  unaligned_cast<uint32_t>(header, data_offset+1) ;
        data_offset += 5;
        return;
      case string_header_NA:
        r_string_len = NA_STRING_LENGTH;
        data_offset += 1;
        return;
      }
    } 
    throw exception("something went wrong (reading string header)");
  }
  void decompress_direct(char* bpointer) {
    block_i++;
    std::array<char, 4> zsize_ar = {0,0,0,0};
    myFile.read(zsize_ar.data(), 4);
    uint64_t zsize = *reinterpret_cast<uint32_t*>(zsize_ar.data());
    myFile.read(zblock.data(), zsize);
    block_size = ZSTD_decompress(bpointer, BLOCKSIZE, zblock.data(), zsize);
  }
  void decompress_block() {
    block_i++;
    std::array<char, 4> zsize_ar = {0,0,0,0};
    myFile.read(zsize_ar.data(), 4);
    uint64_t zsize = *reinterpret_cast<uint32_t*>(zsize_ar.data());
    myFile.read(zblock.data(), zsize);
    block_size = ZSTD_decompress(block.data(), BLOCKSIZE, zblock.data(), zsize);
    data_offset = 0;
  }
  void getBlockData(char* outp, uint64_t data_size) {
    if(data_size <= block_size - data_offset) {
      memcpy(outp, block.data()+data_offset, data_size);
      data_offset += data_size;
    } else {
      uint64_t bytes_accounted = block_size - data_offset;
      memcpy(outp, block.data()+data_offset, bytes_accounted);
      while(bytes_accounted < data_size) {
        if(data_size - bytes_accounted >= BLOCKSIZE) {
          decompress_direct(outp+bytes_accounted);
          bytes_accounted += BLOCKSIZE;
        } else {
          decompress_block();
          memcpy(outp + bytes_accounted, block.data(), data_size - bytes_accounted);
          data_offset = data_size - bytes_accounted;
          bytes_accounted += data_offset;
        }
      }
    }
  }
  SEXP processBlock() {
    SEXPTYPE obj_type;
    uint64_t r_array_len;
    readHeader(obj_type, r_array_len);
    uint64_t number_of_attributes;
    if(obj_type == ANYSXP) {
      number_of_attributes = r_array_len;
      readHeader(obj_type, r_array_len);
    } else {
      number_of_attributes = 0;
    }
    SEXP obj;
    switch(obj_type) {
    case VECSXP: 
      obj = PROTECT(Rf_allocVector(VECSXP, r_array_len));
      for(uint64_t i=0; i<r_array_len; i++) {
        SET_VECTOR_ELT(obj, i, processBlock());
      }
      break;
    case REALSXP:
      obj = PROTECT(Rf_allocVector(REALSXP, r_array_len));
      if(r_array_len > 0) getBlockData(reinterpret_cast<char*>(REAL(obj)), r_array_len*8);
      break;
    case INTSXP:
      obj = PROTECT(Rf_allocVector(INTSXP, r_array_len));
      if(r_array_len > 0) getBlockData(reinterpret_cast<char*>(INTEGER(obj)), r_array_len*4);
      break;
    case LGLSXP:
      obj = PROTECT(Rf_allocVector(LGLSXP, r_array_len));
      if(r_array_len > 0) getBlockData(reinterpret_cast<char*>(LOGICAL(obj)), r_array_len*4);
      break;
    case RAWSXP:
      obj = PROTECT(Rf_allocVector(RAWSXP, r_array_len));
      if(r_array_len > 0) getBlockData(reinterpret_cast<char*>(RAW(obj)), r_array_len);
      break;
    case STRSXP:
      
        obj = PROTECT(Rf_allocVector(STRSXP, r_array_len));
        for(uint64_t i=0; i<r_array_len; i++) {
          uint32_t r_string_len;
          cetype_t string_encoding = CE_NATIVE;
          readStringHeader(r_string_len, string_encoding);
          if(r_string_len == NA_STRING_LENGTH) {
            SET_STRING_ELT(obj, i, NA_STRING);
          } else if(r_string_len == 0) {
            SET_STRING_ELT(obj, i, Rf_mkCharLen("", 0));
          } else if(r_string_len > 0) {
            if(r_string_len > temp_string.size()) {
              temp_string.resize(r_string_len);
            }
            getBlockData(&temp_string[0], r_string_len);
            SET_STRING_ELT(obj, i, Rf_mkCharLenCE(temp_string.data(), r_string_len, string_encoding));
          }
        }
      
      break;
    case CPLXSXP:
      obj = PROTECT(Rf_allocVector(CPLXSXP, r_array_len));
      if(r_array_len > 0) getBlockData(reinterpret_cast<char*>(COMPLEX(obj)), r_array_len*16);
      break;
    case S4SXP:
    {
      SEXP obj_data = PROTECT(Rf_allocVector(RAWSXP, r_array_len));
      getBlockData(reinterpret_cast<char*>(RAW(obj_data)), r_array_len);
      obj = PROTECT(unserializeFromRaw(obj_data));
      UNPROTECT(2);
      return obj;
    }
    default: // also NILSXP
      obj = R_NilValue;
      return obj;
    }
    if(number_of_attributes > 0) {
      for(uint64_t i=0; i<number_of_attributes; i++) {
        uint32_t r_string_len;
        cetype_t string_encoding;
        readStringHeader(r_string_len, string_encoding);
        if(r_string_len > temp_string.size()) {
          temp_string.resize(r_string_len);
        }
        std::string attribute_name;
        attribute_name.resize(r_string_len);
        getBlockData(&attribute_name[0], r_string_len);
        Rf_setAttrib(obj, Rf_install(attribute_name.data()), processBlock());
      }
    }
    UNPROTECT(1);
    return std::move(obj);
  }
};


////////////////////////////////////////////////////////////////
// serialization functions
////////////////////////////////////////////////////////////////

struct CompressBuffer {
  uint64_t number_of_blocks = 0;
  std::vector<char> block = std::vector<char>(BLOCKSIZE);
  std::vector<char> zblock = std::vector<char>(ZSTD_compressBound(BLOCKSIZE));
  uint64_t current_blocksize=0;
  std::ofstream & myFile;
  uint64_t compress_level;
  CompressBuffer(std::ofstream & f, int cl) : myFile(f), compress_level(cl) {}
  void flush() {
    if(current_blocksize > 0) {
      uint64_t zsize = ZSTD_compress(zblock.data(), zblock.size(), block.data(), current_blocksize, compress_level);
      writeSizeToFile4(myFile, zsize);
      myFile.write(zblock.data(), zsize);
      current_blocksize = 0;
      number_of_blocks++;
    }
  }
  void append(char* data, uint64_t len, bool contiguous = false) {
    uint64_t current_pointer_consumed = 0;
    while(current_pointer_consumed < len) {
      if( (current_blocksize == BLOCKSIZE) || ((BLOCKSIZE - current_blocksize < BLOCKRESERVE) && !contiguous) ) {
        flush();
      }
      if(current_blocksize == 0 && len - current_pointer_consumed >= BLOCKSIZE) {
        uint64_t zsize = ZSTD_compress(zblock.data(), zblock.size(), data + current_pointer_consumed, BLOCKSIZE, compress_level);
        writeSizeToFile4(myFile, zsize);
        myFile.write(zblock.data(), zsize);
        current_pointer_consumed += BLOCKSIZE;
        number_of_blocks++;
      } else {
        uint64_t remaining_pointer_available = len - current_pointer_consumed;
        uint64_t add_length = remaining_pointer_available < (BLOCKSIZE - current_blocksize) ? remaining_pointer_available : BLOCKSIZE-current_blocksize;
        memcpy(block.data() + current_blocksize, data + current_pointer_consumed, add_length);
        current_blocksize += add_length;
        current_pointer_consumed += add_length;
      }
    }
  }
};

// should this be a member function?
template<typename POD>
inline void vbuf_append_pod(CompressBuffer & vbuf, POD pod, bool contiguous = false) {
  vbuf.append(reinterpret_cast<char*>(&pod), sizeof(pod), contiguous);
}

// write header to vbuf
void writeHeader(CompressBuffer & vbuf, SEXPTYPE object_type, uint64_t length) {
  switch(object_type) {
  case REALSXP:
    if(length < 32) {
      vbuf_append_pod(vbuf, static_cast<unsigned char>( numeric_header_5 | static_cast<unsigned char>(length) ) );
    } else if(length < 256) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&numeric_header_8)), 1);
      vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
    } else if(length < 65536) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&numeric_header_16)), 1);
      vbuf_append_pod(vbuf, static_cast<uint16_t>(length), true );
    } else if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&numeric_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&numeric_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case VECSXP:
    if(length < 32) {
      vbuf_append_pod(vbuf, static_cast<unsigned char>( list_header_5 | static_cast<unsigned char>(length) ) );
    } else if(length < 256) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&list_header_8)), 1);
      vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
    } else if(length < 65536) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&list_header_16)), 1);
      vbuf_append_pod(vbuf, static_cast<uint16_t>(length), true );
    } else if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&list_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&list_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case INTSXP:
    if(length < 32) {
      vbuf_append_pod(vbuf, static_cast<unsigned char>( integer_header_5 | static_cast<unsigned char>(length) ) );
    } else if(length < 256) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&integer_header_8)), 1);
      vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
    } else if(length < 65536) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&integer_header_16)), 1);
      vbuf_append_pod(vbuf, static_cast<uint16_t>(length), true );
    } else if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&integer_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&integer_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case LGLSXP:
    if(length < 32) {
      vbuf_append_pod(vbuf, static_cast<unsigned char>( logical_header_5 | static_cast<unsigned char>(length) ) );
    } else if(length < 256) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&logical_header_8)), 1);
      vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
    } else if(length < 65536) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&logical_header_16)), 1);
      vbuf_append_pod(vbuf, static_cast<uint16_t>(length), true );
    } else if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&logical_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&logical_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case RAWSXP:
    if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&raw_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&raw_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case STRSXP:
    if(length < 32) {
      vbuf_append_pod(vbuf, static_cast<unsigned char>( character_header_5 | static_cast<unsigned char>(length) ) );
    } else if(length < 256) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&character_header_8)), 1);
      vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
    } else if(length < 65536) { 
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&character_header_16)), 1);
      vbuf_append_pod(vbuf, static_cast<uint16_t>(length), true );
    } else if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&character_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&character_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case CPLXSXP:
    if(length < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&complex_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&complex_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(length), true );
    }
    return;
  case NILSXP:
    vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&null_header)), 1);
    return;
  default:
    // should never reach here
    throw exception("something went wrong writing object header");
    // vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&null_header)), 1);
    // return;
  }
}

void writeStringHeader(CompressBuffer & vbuf, uint64_t length, cetype_t ce_enc) {
  unsigned char enc;
  switch(ce_enc) {
  case CE_NATIVE:
    enc = string_enc_native; break;
  case CE_UTF8:
    enc = string_enc_utf8; break;
  case CE_LATIN1:
    enc = string_enc_latin1; break;
  case CE_BYTES:
    enc = string_enc_bytes; break;
  default:
    enc = string_enc_native;
  }
  if(length < 32) {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( string_header_5 | static_cast<unsigned char>(enc) | static_cast<unsigned char>(length) ) );
  } else if(length < 256) {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( string_header_8 | static_cast<unsigned char>(enc) ) );
    vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
  } else if(length < 65536) {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( string_header_16 | static_cast<unsigned char>(enc) ) );
    vbuf_append_pod(vbuf, static_cast<uint16_t>(length), true );
  } else {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( string_header_32 | static_cast<unsigned char>(enc) ) );
    vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
  }
}

void writeAttributeHeader(CompressBuffer & vbuf, uint64_t length) {
  if(length < 32) {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( attribute_header_5 | static_cast<unsigned char>(length) ) );
  } else if(length < 256) {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( attribute_header_8 ) );
    vbuf_append_pod(vbuf, static_cast<uint8_t>(length), true );
  } else {
    vbuf_append_pod(vbuf, static_cast<unsigned char>( attribute_header_32 ) );
    vbuf_append_pod(vbuf, static_cast<uint32_t>(length), true );
  }
}



void appendToVbuf(CompressBuffer & vbuf, RObject & x, bool attributes_processed = false) {
  if(!attributes_processed && stypes.find(TYPEOF(x)) != stypes.end()) {
    std::vector<std::string> anames = x.attributeNames();
    if(anames.size() != 0) {
      writeAttributeHeader(vbuf, anames.size());
      appendToVbuf(vbuf, x, true);
      for(uint64_t i=0; i<anames.size(); i++) {
        writeStringHeader(vbuf, anames[i].size(),CE_NATIVE);
        vbuf.append(&anames[i][0], anames[i].size(), true);
        RObject xa = x.attr(anames[i]);
        appendToVbuf(vbuf, xa);
      }
    } else {
      appendToVbuf(vbuf, x, true);
    }
  } else if(TYPEOF(x) == STRSXP) {
    uint64_t dl = Rf_xlength(x);
    writeHeader(vbuf, STRSXP, dl);
    CharacterVector xc = CharacterVector(x);
    for(uint64_t i=0; i<dl; i++) {
      SEXP xi = xc[i];
      if(xi == NA_STRING) {
        vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&string_header_NA)), 1);
      } else {
        uint64_t dl = LENGTH(xi);
        writeStringHeader(vbuf, dl, Rf_getCharCE(xi));
        vbuf.append(const_cast<char*>(CHAR(xi)), dl, true);
      }
    }
  } else if(stypes.find(TYPEOF(x)) != stypes.end()) {
    uint64_t dl = Rf_xlength(x);
    writeHeader(vbuf, TYPEOF(x), dl);
    if(TYPEOF(x) == VECSXP) {
      List xl = List(x);
      for(uint64_t i=0; i<dl; i++) {
        RObject xi = xl[i];
        appendToVbuf(vbuf, xi);
      }
    } else {
      switch(TYPEOF(x)) {
      case REALSXP:
        vbuf.append(reinterpret_cast<char*>(REAL(x)), dl*8, true); break;
      case INTSXP:
        vbuf.append(reinterpret_cast<char*>(INTEGER(x)), dl*4, true); break;
      case LGLSXP:
        vbuf.append(reinterpret_cast<char*>(LOGICAL(x)), dl*4, true); break;
      case RAWSXP:
        vbuf.append(reinterpret_cast<char*>(RAW(x)), dl, true); break;
      case CPLXSXP:
        vbuf.append(reinterpret_cast<char*>(COMPLEX(x)), dl*16, true); break;
      case NILSXP:
        break;
      }
    }
  } else { // other non-supported SEXPTYPEs use the built in R serialization method
    RawVector xserialized = serializeToRaw(x);
    if(xserialized.size() < 4294967296) {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&nstype_header_32)), 1);
      vbuf_append_pod(vbuf, static_cast<uint32_t>(xserialized.size()), true );
    } else {
      vbuf.append(reinterpret_cast<char*>(const_cast<unsigned char*>(&nstype_header_64)), 1);
      vbuf_append_pod(vbuf, static_cast<uint64_t>(xserialized.size()), true );
    }
    vbuf.append(reinterpret_cast<char*>(RAW(xserialized)), xserialized.size(), true);
  }
}
