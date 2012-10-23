#ifndef BASETYPES_H_
#define BASETYPES_H_

typedef struct {
	float x, y, z; // position, also color (r,g,b)
} Vec;

typedef struct {
	/* User defined values */
	Vec orig, target;
	/* Calculated values */
	Vec dir, x, y;
} Camera;

typedef struct {
	Vec o, d;
} Ray;

enum Refl {
	DIFF, SPEC, REFR
}; /* material types, used in radiance() */

typedef struct {
	float rad; /* radius */
	Vec p, e, c; /* position, emission, color */
	enum Refl refl; /* reflection type (DIFFuse, SPECular, REFRactive) */
} Sphere;

#endif /* BASETYPES_H_ */
