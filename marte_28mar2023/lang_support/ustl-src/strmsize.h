// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
/// \file strmsize.h
/// \brief This file contains stream_size_of functions for basic types and *STREAMABLE macros.
/// stream_size_of functions return the size of the object's data that is written or
/// read from a stream.
//

#ifndef STRMSIZE_H_052FF16B2D8A608761BF10333D065073
#define STRMSIZE_H_052FF16B2D8A608761BF10333D065073

namespace ustl {

/// For partial specialization of stream_size_of for objects
template <typename T> struct object_stream_size {
    inline size_t operator()(const T& v) const { return (v.stream_size()); }
};
template <typename T> struct integral_object_stream_size {
    inline size_t operator()(const T& v) const { return (sizeof(v)); }
};
/// Returns the size of the given object. Overloads for standard types are available.
template <typename T>
inline size_t stream_size_of (const T& v) {
    typedef typename tm::Select <numeric_limits<T>::is_integral,
	integral_object_stream_size<T>, object_stream_size<T> >::Result stream_sizer_t;
    return (stream_sizer_t()(v));
}

} // namespace ustl

//
// Extra overloads in this macro are needed because it is the one used for
// marshalling pointers. Passing a pointer to stream_size_of creates a
// conversion ambiguity between converting to const pointer& and converting
// to bool; the compiler always chooses the bool conversion (because it
// requires 1 conversion instead of 2 for the other choice). There is little
// point in adding the overloads to other macros, since they are never used
// for pointers.
//
/// Declares that T is to be written as is into binary streams.
#define INTEGRAL_STREAMABLE(T)	\
    namespace ustl {		\
	inline istream& operator>> (istream& is, T& v)		{ is.iread(v);  return (is); }	\
	inline ostream& operator<< (ostream& os, const T& v)	{ os.iwrite(v); return (os); }	\
	inline ostream& operator<< (ostream& os, T& v)		{ os.iwrite(v); return (os); }	\
	template <> inline size_t stream_size_of (const T& v)	{ return (sizeof(v)); }		\
    }

/// Declares that T contains read, write, and stream_size methods. This is no longer needed and is deprecated.
#define STD_STREAMABLE(T)

/// Declares \p T to be writable to text streams. This is no longer needed and is deprecated.
#define TEXT_STREAMABLE(T)

/// Declares that T is to be cast into TSUB for streaming.
#define CAST_STREAMABLE(T,TSUB)	\
    namespace ustl {		\
	inline istream& operator>> (istream& is, T& v)		{ TSUB sv; is >> sv; v = (T)(sv); return (is); }	\
	inline ostream& operator<< (ostream& os, const T& v)	{ os << TSUB(v); return (os); }				\
	template <> inline size_t stream_size_of (const T& v)	{ return (stream_size_of (TSUB(v))); }			\
    }

/// Placed into a class it declares the methods required by STD_STREAMABLE. Syntactic sugar.
#define DECLARE_STD_STREAMABLE			\
    public:					\
	void	read (istream& is);		\
	void	write (ostream& os) const;	\
	size_t	stream_size (void) const

/// Specifies that \p T is printed by using it as an index into \p Names string array.
#define LOOKUP_TEXT_STREAMABLE(T,Names,nNames)	\
    namespace ustl {		\
	inline ostringstream& operator<< (ostringstream& os, const T& v)	\
	{				\
	    os << Names[min(uoff_t(v),uoff_t(nNames-1))];	\
	    return (os);		\
	}				\
    }

#endif

