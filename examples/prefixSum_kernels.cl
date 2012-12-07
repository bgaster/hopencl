__kernel void square(__global int * in) {
	size_t i = get_global_id(0);
	in[i] *= in[i];
}

__kernel void prefixSumStep(int iteration, __global int * in, __global int * out) {
	size_t i = get_global_id(0);	
	int j = (int)exp2((float)iteration);
	if (i >= j)
		out[i] = in[i] + in[i-j];
	else
		out[i] = in[i];
}
