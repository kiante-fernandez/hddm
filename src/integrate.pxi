#cython: embedsignature=True
#cython: cdivision=True
#cython: wraparound=False
#cython: boundscheck=False

import numpy as np
cimport numpy as np
cimport cython

include 'pdf.pxi'

cdef double simpson_1D(double x, double v, double sv, double a, double z, double t, double err,
                        double lb_a, double ub_a, int n_sa, double lb_t, double ub_t, int n_st) nogil:
    #assert ((n_sa&1)==0 and (n_st&1)==0), "n_st and n_sa have to be even"
    #assert ((ub_t-lb_t)*(ub_a-lb_a)==0 and (n_sa*n_st)==0), "the function is defined for 1D-integration only"

    cdef double ht, ha
    cdef int n = max(n_st, n_sa)
    if n_st==0: #integration over a, note we want to integrate over a gamma, not a uniform. This is a for now case.
        ha = (ub_a-lb_a)/n
        ht = 0
        lb_t = t
        ub_t = t
    else: #integration over t
        ha = 0
        ht = (ub_t-lb_t)/n
        lb_a = a
        ub_a = a

    cdef double S = pdf_sv(x - lb_t, v, sv, lb_a, z, err)
    cdef double a_tag, t_tag, y
    cdef int i

    for i from 1 <= i <= n:
        a_tag = lb_a + ha * i
        t_tag = lb_t + ht * i
        y = pdf_sv(x - t_tag, v, sv, a_tag, z, err)
        if i&1: #check if i is odd
            S += (4 * y)
        else:
            S += (2 * y)
    S = S - y #the last term should be f(b) and not 2*f(b) so we subtract y
    S = S / ((ub_t-lb_t)+(ub_a-lb_a)) #the right function if pdf_sv()/sa or pdf_sv()/st

    return ((ht+ha) * S / 3)

cdef double simpson_2D(double x, double v, double sv, double a, double z, double t, double err, double lb_a, double ub_a, int n_sa, double lb_t, double ub_t, int n_st) nogil:
    #assert ((n_sa&1)==0 and (n_st&1)==0), "n_st and n_sa have to be even"
    #assert ((ub_t-lb_t)*(ub_a-lb_a)>0 and (n_sa*n_st)>0), "the function is defined for 2D-integration only, lb_t: %f, ub_t %f, lb_z %f, ub_z %f, n_sa: %d, n_st %d" % (lb_t, ub_t, lb_a, ub_a, n_sa, n_st)

    cdef double ht
    cdef double S
    cdef double t_tag, y
    cdef int i_t

    ht = (ub_t-lb_t)/n_st

    S = simpson_1D(x, v, sv, a, z, lb_t, err, lb_a, ub_a, n_sa, 0, 0, 0)

    for i_t  from 1 <= i_t <= n_st:
        t_tag = lb_t + ht * i_t
        y = simpson_1D(x, v, sv, a, z, t_tag, err, lb_a, ub_a, n_sa, 0, 0, 0)
        if i_t&1: #check if i is odd
            S += (4 * y)
        else:
            S += (2 * y)
    S = S - y #the last term should be f(b) and not 2*f(b) so we subtract y
    S = S/ (ub_t-lb_t)

    return (ht * S / 3)


cdef double adaptiveSimpsonsAux(double x, double v, double sv, double a, double z, double t, double pdf_err,
                                 double lb_a, double ub_a, double lb_t, double ub_t, double AT, double simps_err,
                                 double S, double f_beg, double f_end, double f_mid, int bottom) nogil:

    cdef double a_c, a_d, a_e, t_c, t_d, t_e, h
    cdef double fd, fe
    cdef double Sleft, Sright, S2
    #print "in AdaptiveSimpsAux: lb_z: %f, ub_z: %f, lb_t %f, ub_t %f, f_beg: %f, f_end: %f, bottom: %d" % (lb_z, ub_z, lb_t, ub_t, f_beg, f_end, bottom)

    if (ub_t-lb_t) == 0: #integration over sa
        h = ub_a - lb_a
        a_c = (ub_a + lb_a)/2.
        a_d = (lb_a + a_c)/2.
        a_e = (a_c  + ub_a)/2.
        t_c = t
        t_d = t
        t_e = t

    else: #integration over t
        h = ub_t - lb_t
        t_c = (ub_t + lb_t)/2.
        t_d = (lb_t + t_c)/2.
        t_e = (t_c  + ub_t)/2.
        a_c = a
        a_d = a
        a_e = a

    fd = pdf_sv(x - t_d, v, sv, a_d, z, pdf_err)/AT
    fe = pdf_sv(x - t_e, v, sv, a_e, z, pdf_err)/AT

    Sleft = (h/12)*(f_beg + 4*fd + f_mid)
    Sright = (h/12)*(f_mid + 4*fe + f_end)
    S2 = Sleft + Sright
    if (bottom <= 0 or fabs(S2 - S) <= 15*simps_err):
        return S2 + (S2 - S)/15
    return adaptiveSimpsonsAux(x, v, sv, a, z, t, pdf_err,
                                 lb_a, a_c, lb_t, t_c, AT, simps_err/2,
                                 Sleft, f_beg, f_mid, fd, bottom-1) + \
            adaptiveSimpsonsAux(x, v, sv, a, z, t, pdf_err,
                                 a_c, ub_a, t_c, ub_t, AT, simps_err/2,
                                 Sright, f_mid, f_end, fe, bottom-1)

cdef double adaptiveSimpsons_1D(double x, double v, double sv, double a, double z, double t,
                              double pdf_err, double lb_a, double ub_a, double lb_t, double ub_t,
                              double simps_err, int maxRecursionDepth) nogil:

    cdef double h

    if (ub_t - lb_t) == 0: #integration over a
        lb_t = t
        ub_t = t
        h = ub_a - lb_a
    else: #integration over t
        h = (ub_t-lb_t)
        lb_a = a
        ub_a = a

    cdef double AT = h
    cdef double c_t = (lb_t + ub_t)/2.
    cdef double c_a = (lb_a + ub_a)/2.

    cdef double f_beg, f_end, f_mid, S
    f_beg = pdf_sv(x - lb_t, v, sv, lb_a, z, pdf_err)/AT
    f_end = pdf_sv(x - ub_t, v, sv, ub_a, z, pdf_err)/AT
    f_mid = pdf_sv(x - c_t, v, sv, c_a, z, pdf_err)/AT
    S = (h/6)*(f_beg + 4*f_mid + f_end)
    cdef double res =  adaptiveSimpsonsAux(x, v, sv, a, z, t, pdf_err,
                                 lb_a, ub_a, lb_t, ub_t, AT, simps_err,
                                 S, f_beg, f_end, f_mid, maxRecursionDepth)
    return res

cdef double adaptiveSimpsonsAux_2D(double x, double v, double sv,
                                   double a, double z, double t, double
                                   pdf_err, double err_1d, double lb_a,
                                   double ub_a, double lb_t, double
                                   ub_t, double st, double err_2d, double
                                   S, double f_beg, double f_end, double
                                   f_mid, int maxRecursionDepth_sa, int
                                   bottom) nogil:

    cdef double fd, fe
    cdef double Sleft, Sright, S2
    #print "in AdaptiveSimpsAux_2D: lb_z: %f, ub_z: %f, lb_t %f, ub_t %f, f_beg: %f, f_end: %f, bottom: %d" % (lb_z, ub_z, lb_t, ub_t, f_beg, f_end, bottom)

    cdef double t_c = (ub_t + lb_t)/2.
    cdef double t_d = (lb_t + t_c)/2.
    cdef double t_e = (t_c  + ub_t)/2.
    cdef double h = ub_t - lb_t

    fd = adaptiveSimpsons_1D(x, v, sv, a, z, t_d, pdf_err, lb_a, ub_a,
                              0, 0, err_1d, maxRecursionDepth_sa)/st
    fe = adaptiveSimpsons_1D(x, v, sv, a, z, t_e, pdf_err, lb_a, ub_a,
                              0, 0, err_1d, maxRecursionDepth_sa)/st

    Sleft = (h/12)*(f_beg + 4*fd + f_mid)
    Sright = (h/12)*(f_mid + 4*fe + f_end)
    S2 = Sleft + Sright

    if (bottom <= 0 or fabs(S2 - S) <= 15*err_2d):
        return S2 + (S2 - S)/15;

    return adaptiveSimpsonsAux_2D(x, v, sv, a, z, t, pdf_err, err_1d,
                                 lb_a, ub_a, lb_t, t_c, st, err_2d/2,
                                 Sleft, f_beg, f_mid, fd, maxRecursionDepth_sa, bottom-1) + \
            adaptiveSimpsonsAux_2D(x, v, sv, a, z, t, pdf_err, err_1d,
                                 lb_a, ub_a, t_c, ub_t, st, err_2d/2,
                                 Sright, f_mid, f_end, fe, maxRecursionDepth_sa, bottom-1)


cdef double adaptiveSimpsons_2D(double x, double v, double sv, double a, double z, double t,
                                 double pdf_err, double lb_a, double ub_a, double lb_t, double ub_t,
                                 double simps_err, int maxRecursionDepth_sa, int maxRecursionDepth_st) nogil:

    cdef double h = (ub_t-lb_t)

    cdef double st = (ub_t - lb_t)
    cdef double c_t = (lb_t + ub_t)/2.
    cdef double c_a = (lb_a + ub_a)/2.

    cdef double f_beg, f_end, f_mid, S
    cdef double err_1d = simps_err
    cdef double err_2d = simps_err

    f_beg = adaptiveSimpsons_1D(x, v, sv, a, z, lb_t, pdf_err, lb_a, ub_a,
                              0, 0, err_1d, maxRecursionDepth_sa)/st

    f_end = adaptiveSimpsons_1D(x, v, sv, a, z, ub_t, pdf_err, lb_a, ub_a,
                              0, 0, err_1d, maxRecursionDepth_sa)/st
    f_mid = adaptiveSimpsons_1D(x, v, sv, a, z, (lb_t+ub_t)/2, pdf_err, lb_a, ub_a,
                              0, 0, err_1d, maxRecursionDepth_sa)/st
    S = (h/6)*(f_beg + 4*f_mid + f_end)
    cdef double res =  adaptiveSimpsonsAux_2D(x, v, sv, a, z, t, pdf_err, err_1d,
                                 lb_a, ub_a, lb_t, ub_t, st, err_2d,
                                 S, f_beg, f_end, f_mid, maxRecursionDepth_sa, maxRecursionDepth_st)
    return res
