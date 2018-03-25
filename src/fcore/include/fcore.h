/*
 * gfortran up to v5.x incorrectly processes procedure calls if the actual
 * argument is a temporary array of user-derived type, and the dummy argument
 * is polymorphic; see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60322
 */

#if __GFORTRAN__ && (__GNUC__  < 6)

#define __FCORE_POLY_ARRAY(t) type (t)

#else

#define __FCORE_POLY_ARRAY(t) class (t)

#endif

