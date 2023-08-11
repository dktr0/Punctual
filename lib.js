var h$ghcjsbn_zero_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (0)));;
var h$ghcjsbn_one_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (1)));;
var h$ghcjsbn_negOne_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-1)));;
var h$ghcjsbn_null_b = [-1];
var h$ghcjsbn_zero_b = [0];
var h$ghcjsbn_one_b = [1, 1];
var h$ghcjsbn_two31_b = [2, 0, 8];
var h$ghcjsbn_czero_b = [2, 268435455, 15];
var h$ghcjsbn_two31_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (h$ghcjsbn_two31_b)));;
var h$ghcjsbn_negTwo31_i = (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-2147483648)));;
var h$ghcjsbn_smallPrimes =
 [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47
 , 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113
 , 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197
 , 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281
 , 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379
 , 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463
 , 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571
 , 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659
 , 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761
 , 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863
 , 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977
 , 983, 991, 997
 ];
var h$ghcjsbn_smallPrimesM = null;
function h$ghcjsbn_getSmallPrimesM() {
  var a, i;
  if(h$ghcjsbn_smallPrimesM === null) {
    a = [];
    for(i = 0; i < 1008; i++) {
      a[i] = false;
    }
    for(i = h$ghcjsbn_smallPrimes.length - 1; i >= 0; i--) {
      a[h$ghcjsbn_smallPrimes[i]] = true;
    }
    h$ghcjsbn_smallPrimesM = a;
  }
  return h$ghcjsbn_smallPrimesM;
}
function h$ghcjsbn_isPrime_s(s, rounds) {
  if(s < 2 || (s > 2 && ((s&1) === 1))) return false;
  if(s <= 1008) {
    return h$ghcjsbn_getSmallPrimesM()[s];
  }
  throw new Error("isPrime_s");
}
function h$ghcjsbn_random_b(min, max) {
  if(h$ghcjsbn_cmp_bb(min, max) >= 0) {
    return min;
  }
  var range = h$ghcjsbn_sub_bb(max, min);
  var size = 4 + Math.ceil(h$ghcjsbn_nbits_b(range) / 16);
  var r = h$ghcjsbn_zero_b;
  for(var i=0;i<size;i++) {
    var rnd = Math.floor(Math.random()*65536);
    r = h$ghcjsbn_or_bb(h$ghcsbn_shl_b(r, 16), h$ghcjsbn_mkBigNat_w(rnd));
  }
  return h$ghcjsbn_add_bb(h$ghcjsbn_rem_bb(r, range), min);
}
function h$ghcjsbn_testPrime_b(n, rounds) {
  var r = 0, d = n;
  var nm1 = h$ghcjsbn_sub_bw(n,1);
  var two = h$ghcjsbn_mkBigNat_w(2);
  while(h$ghcjsbn_testBit_b(d, 0)) {
    d = h$ghcjsbn_shr_b(d, 1);
    r++;
  }
  for(var round = 0; round < rounds; round++) {
    var a = h$ghcjsbn_random_b(two, nm1);
    var x = h$ghcjsbn_modPow_bbb(a, d, n);
    if(h$ghcjsbn_eq_bw(x,1) || h$ghcjsbn_eq_bb(x,nm1)) continue;
    var found = false;
    for(var rr=0;rr<r;rr++) {
      x = h$ghcjsbn_modPow_bbb(x, two, n);
      if(h$ghcjsbn_eq_bb(x, nm1)) {
        found = true;
        break;
      }
    }
    if(!found) return 0;
  }
  return 1;
}
function h$ghcjsbn_testPrime_w(w, rounds) {
  return h$ghcjsbn_testPrime_b(h$ghcjsbn_mkBigNat_w(w), rounds);
}
function h$integer_gmp_next_prime1(w) {
  var r = h$ghcjsbn_nextPrime_b(h$ghcjsbn_mkBigNat_w(w));
  return h$ghcjsbn_toWord_b(r);
}
function h$ghcjsbn_nextPrime_b(bn) {
  var rounds = 64;
  if(h$ghcjsbn_eq_bw(bn, 2)) {
    return h$ghcjsbn_mkBigNat_w(3);
  }
  var i = bn;
  do {
    i = h$ghcjsbn_add_bw(i, 2);
  } while(!h$ghcjsbn_testPrime_b(i, rounds));
  return i;
}
function h$ghcjsbn_cmp_bb(b1, b2) {
  ;
  ;
  var l1 = b1[0], l2 = b2[0], d1, d2;
  if(l1 === l2) {
    while(--l1 >= 0) {
      d1 = b1[l1+1];
      d2 = b2[l1+1];
      if(d1 !== d2) return d1 < d2 ? 0 : 2;
    }
    return 1;
  } else {
    return l1 > l2 ? 2 : 0;
  }
}
var h$ghcjsbn_tmp_2a = [0, 0, 0];
var h$ghcjsbn_tmp_2b = [0, 0, 0];
var h$ghcjsbn_tmp_a = [0, 0, 0, 0, 0, 0, 0, 0];
var h$ghcjsbn_tmp_b = [0, 0, 0, 0, 0, 0, 0, 0];
function h$ghcjsbn_sub_bw(b, w) {
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  return h$ghcjsbn_sub_bb(b, a);
}
function h$ghcjsbn_sub_bs(b, s) {
  ;
  ;
  var a, ms, r;
  if(s < 0) {
    if(s === -2147483648) {
      r = h$ghcjsbn_add_bb(b, h$ghcjsbn_two31_b);
    } else {
      a = h$ghcjsn_tmp_2a;
      h$ghcjsbn_toBigNat_s(a, -s);
      r = h$ghcjsbn_add_bb(b, a);
    }
  } else {
    a = h$ghcjsn_tmp_2a;
    h$ghcjsbn_toBigNat_s(a, s);
    r = h$ghcjsbn_sub_bb(b, a);
  }
  ;
  return r;
}
function h$ghcjsbn_add_bw(b, w) {
  ;
  ;
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  return h$ghcjsbn_add_bb(b, a);
}
function h$ghcjsbn_add_bs(b, s) {
  ;
  ;
  var a, ms, r;
  if(s < 0) {
    if(s === -2147483648) {
      r = h$ghcjsbn_sub_bb(b, h$ghcjsbn_two31_r);
    } else {
      ms = -s;
      a = h$ghcjsbn_tmp_2a;
      h$ghcjsbn_toBigNat_s(a, ms);
      r = h$ghcjsbn_sub(b, a);
    }
  } else {
    a = h$ghcjsbn_tmp_2a;
    h$ghcjsbn_toBigNat_s(a, s);
    r = h$ghcjsbn_add_bb(b, a);
  }
  ;
  return r;
}
function h$ghcjsbn_add_bb(b1, b2) {
  ;
  ;
  var i, c = 0, l1 = b1[0], l2 = b2[0], t = [0];
  var bl, lmin, lmax;
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    bl = b2;
  } else {
    lmin = l2;
    lmax = l1;
    bl = b1;
  }
  for(i=1;i<=lmin;i++) {
    c += b1[i] + b2[i];
    t[i] = c & 0xfffffff;
    c >>= 28;
  }
  for(i=lmin+1;i<=lmax;i++) {
    c += bl[i];
    t[i] = c & 0xfffffff;
    c >>= 28;
  }
  if(c !== 0) t[++lmax] = c;
  t[0] = lmax;
  ;
  return t;
}
function h$ghcjsbn_addTo_bb(b1, b2) {
  ;
  ;
  var i, c = 0, l1 = b1[0], l2 = b2[0];
  if(l2 > l1) {
    for(i = l1 + 1; i <= l2; i++) {
      b1[i] = 0;
    }
    l1 = l2;
  }
  for(i = 1; i <= l2; i++) {
    c += b1[i] + b2[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  for(i = l2 + 1; c !== 0 && i <= l1; i++) {
    c += b1[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  if(c !== 0) {
    b1[l1] = c;
    b1[0] = l1+1;
  } else {
    b1[0] = l1;
  }
  ;
}
function h$ghcjsbn_sub_bb(b1, b2) {
  ;
  ;
  if(h$ghcjsbn_cmp_bb(b1,b2) === 0) {
    return [];
  } else {
    var i, c = 0, l1 = b1[0], l2 = b2[0], t = [0];
    for(i = 1; i <= l2; i++) {
      c += b1[i] - b2[i];
      t[i] = c & 0xfffffff;
      c >>= 28;
    }
    for(i = l2 + 1; i <= l1; i++) {
      c += b1[i];
      t[i] = c & 0xfffffff;
      c >>= 28;
    }
    while(l1 > 0 && t[l1] === 0) l1--;
    t[0] = l1;
    ;
    return t;
  }
}
function h$ghcjsbn_subTo_bb(b1, b2) {
  ;
  ;
  var i, c = 0, l1 = b1[0], l2 = b2[0];
  for(i = 1; i <= l2; i++) {
    c += b1[i] - b2[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  for(i = l2 + 1; c !== 0 && i <= l1; i++) {
    c += b1[i];
    b1[i] = c & 0xfffffff;
    c >>= 28;
  }
  while(l1 > 0 && b1[l1] === 0) l1--;
  b1[0] = l1;
  ;
}
function h$ghcjsbn_wrap_p(b) {
  var l = b[0];
  if(l === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (0)));;
  } else if(l === 1) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (b[1])));;
  } else if(l === 2 && (b[2] >> (31 - 28)) === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, ((b[2] << 28)|b[1])));;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (b)));;
  }
}
function h$ghcjsbn_wrap_n(b) {
  var l = b[0];
  if(l === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (0)));;
  } else if(l === 1) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-b[1])));;
  } else if(l === 2 &&
            ((b[2] >> (31 - GHCJSN_BITS)) === 0 ||
             (b[2] === (1 << (31 - 28)) && b[1] === 0))) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, ((-b[2]-b[1])|0)));;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (b)));;
  }
}
function h$ghcjsbn_mulTo_bb(b1, b2) {
  ;
  ;
  var t = h$ghcjsbn_mul_bb(b1, b2);
  h$ghcjsbn_copy(b1, t);
  ;
}
function h$ghcjsbn_mul_bb(b1, b2) {
  ;
  ;
  var l1 = b1[0], l2 = b2[0];
  var n = l1 + l2, i, t = [0];
  for(i = 1; i <= n; i++) t[i] = 0;
  if(l1 > l2) {
    for(i = 0; i < l2; i++) {
      t[i + l1 + 1] = h$ghcjsbn_mul_limb(0, b1, b2[i+1], t, i, 0, l1);
    }
  } else {
    for(i = 0; i < l1; i++) {
      t[i + l2 + 1] = h$ghcjsbn_mul_limb(0, b2, b1[i+1], t, i, 0, l2);
    }
  }
  for(i = l1 + l2; i > 0 && t[i] === 0; i--);
  t[0] = i;
  ;
  return t;
}
function h$ghcjsbn_mul_bw(b, w) {
  ;
  ;
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  var t = h$ghcjsbn_mul_bb(b, a);
  ;
  return t;
}
function h$ghcjsbn_mul_karatsuba_bb(t, b1, b2) {
  throw new Error("not yet updated");
  var l1 = b1.length, l2 = b2.length;
  var i, b = (l1 < l2 ? l1 : l2) >> 1;
  var x0 = [b], x1 = [l1-b], y0 = [b], y1 = [l2-b];
  for(i = 1; i <= b; i++) {
    x0[i] = b1[i];
    y0[i] = b2[i];
  }
  for(i = b + 1; i <= l1; i++) x1[i - b] = b1[i];
  for(i = b + 1; i <= l2; i++) y1[i - b] = b2[i];
  var z0 = h$ghcjsbn_mul_bb(x0, y0), z1, z2 = h$ghcjsbn_mul_bb(x1, y1);
  h$ghcjsbn_addTo_bb(x0, x1);
  h$ghcjsbn_addTo_bb(y0, x1);
  z1 = h$ghcjsbn_mul_bb(x0, y0);
  h$ghcjsbn_subTo_bb(z1, z2);
  h$ghcjsbn_subTo_bb(z1, z0);
  for(i = 0; i < 2*b; i++) t[i] = 0;
  l2 = z2.length;
  for(i = 0; i < l2; i++) t[i+2*b] = z2[i];
  var z1s = [];
  l1 = z1.length;
  for(i = 0; i < b; i++) z1s[i] = 0;
  for(i = 0; i < l1; i++) z1s[i+b] = z1[i];
  h$ghcjsbn_addTo_bb(t, z1s);
  h$ghcjsbn_addTo_bb(t, z0);
  return t;
}
function h$ghcjsbn_mul_limb(i,b,x,w,j,c,n) {
  var xl = x & 0x3fff, xh = x >> 14;
  while(--n >= 0) {
    var l = b[++i] & 0x3fff;
    var h = b[i] >> 14;
    var m = xh * l + h * xl;
    l = xl *l + ((m & 0x3fff) << 14) + w[++j] + c;
    c = (l >> 28) + (m >> 14) + xh * h;
    w[j] = l & 0xfffffff;
  }
  return c;
}
function h$ghcjsbn_quotRem_bb(q, r, b1, b2) {
  ;
  ;
  if(q === null) q = h$ghcjsbn_tmp_a;
  if(r === null) r = h$ghcjsbn_tmp_b;
  var l1 = b1[0], l2 = b2[0], nsh, y = [];
  if(l1 === 0) {
    q[0] = 0;
    r[0] = 0;
    return;
  }
  if(h$ghcjsbn_cmp_bb(b1,b2) === 0) {
    q[0] = 0;
    h$ghcjsbn_copy(r, b1);
    return;
  }
  nsh = 28 -h$ghcjsbn_nbits_s(b2[l2]);
  ;
  if(nsh !== 0) {
    h$ghcjsbn_shlTo_b(y, b2, nsh);
    h$ghcjsbn_shlTo_b(r, b1, nsh);
  } else {
    h$ghcjsbn_copy(y, b2);
    h$ghcjsbn_copy(r, b1);
  }
  ;
  ;
  var ys = y[0], y0 = y[ys];
  var yt = y0*(1<<24)+((ys>1)?y[ys-1]>>4:0);
  var d1 = 4503599627370496/yt, d2 = (1<<24)/yt, e = 1 << 4;
  var i = r[0], j = i-ys, t = q;
  h$ghcjsbn_shlTo_limbs_b(t,y,j);
  if(h$ghcjsbn_cmp_bb(r, t) !== 0) {
    r[r[0]+1] = 1;
    r[0] += 1;
    h$ghcjsbn_subTo_bb(r, t);
  }
  h$ghcjsbn_shlTo_limbs_b(t, h$ghcjsbn_one_b, ys);
  y = h$ghcjsbn_sub_bb(t, y);
  while(y.length <= ys) y[y.length] = 0;
  while(--j >= 0) {
    var qd = (r[(--i)+1]===y0)?0xfffffff:Math.floor(r[i+1]*d1+(r[i]+e)*d2);
    var am = h$ghcjsbn_mul_limb(0, y, qd, r, j, 0, ys);
    if((r[i+1] += am) < qd) {
      h$ghcjsbn_shlTo_limbs_b(t, y, j);
      h$ghcjsbn_subTo_bb(r, t);
      while(r[i+1] < --qd) {
        h$ghcjsbn_subTo_bb(r, t);
      }
    }
  }
  ;
  h$ghcjsbn_shrTo_limbs_b(q, r, ys);
  r[0] = ys;
  while(r[r[0]] === 0 && r[0] > 0 && r[0]--);
  if(nsh !== 0) {
    var r0 = [];
    h$ghcjsbn_copy(r0, r);
    h$ghcjsbn_shrTo_b(r, r0, nsh);
  }
  ;
  ;
}
function h$ghcjsbn_quotRem_bw(q, b, w) {
  ;
  ;
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  var r = [];
  h$ghcjsbn_quotRem_bb(q, r, b, a);
  return h$ghcjsbn_toWord_b(r);
}
function h$ghcjsbn_tmp_toJSBN(b) {
  var j = new BigInteger(), bl = b[0], i;
  for(i = 0; i < bl; i++) j.data[i] = b[i+1];
  j.s = 0;
  j.t = bl;
  return j;
}
function h$ghcjsbn_tmp_fromJSBN(b, j) {
  var bl = j.t, i;
  for(i = 0; i < bl; i++) {
    b[i] = j.data[i];
  }
  return bl;
}
function h$ghcjsbn_rem_bb(b1, b2) {
  ;
  ;
  var t1 = [], t2 = [];
  h$ghcjsbn_quotRem_bb(t1, t2, b1, b2);
  ;
  return t2;
}
function h$ghcjsbn_rem_bw(b, w) {
  ;
  ;
  var r = h$ghcjsbn_quotRem_bw([] , b, w);
  ;
  return r;
}
function h$ghcjsbn_quot_bb(b1, b2) {
  ;
  ;
  var t1 = [], t2 = [];
  h$ghcjsbn_quotRem_bb(t1, t2, b1, b2);
  ;
  return t1;
}
function h$ghcjsbn_sqr_b(b) {
  ;
  var l = b[0], n = 2 * l, i, c, t = [0];
  for(i = 1; i <= n; i++) t[i] = 0;
  for(i = 0; i < l - 1; i++) {
    c = h$ghcjsbn_mul_limb(i, b, b[i+1],t,2*i,0,1);
    if((t[i + l + 1] += h$ghcjsbn_mul_limb(i+1, b, 2*b[i+1], t, 2*i+1, c, l - i - 1)) >= 0x10000000) {
      t[i + l + 1] -= 0x10000000;
      t[i + l + 2] = 1;
    }
  }
  if(n > 0) t[n] += h$ghcjsbn_mul_limb(i, b, b[i+1], t, 2*i, 0, 1);
  if(t[n] === 0) n--;
  t[0] = n;
  ;
  return t;
}
function h$ghcjsbn_pow_bb(b1, b2) {
  ;
  ;
  var i, sq = b1, t = [1,1];
  var bits = h$ghcjsbn_nbits_b(b2);
  for(i = 0; i < bits; i++) {
    if(h$ghcjsbn_testBit_b(b2, i)) {
      h$ghcjsbn_mulTo_bb(t, sq);
    }
    sq = h$ghcjsbn_sqr_b(sq);
  }
  return t;
}
function h$ghcjsbn_pow_bw(b, w) {
  ;
  ;
  var i, sq = b, t = [1,1];
  while(w) {
    if(w&1) h$ghcjsbn_mulTo_bb(t, sq);
    w >>>= 1;
    if(w) {
      sq = h$ghcjsbn_sqr_b(sq);
    }
  }
  ;
  return t;
}
function h$ghcjsbn_pow_ww(w1, w2) {
  ;
  ;
  var b = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(b, w1);
  var t = h$ghcjsbn_pow_bw(b, w2);
  ;
  return t;
}
function h$ghcjsbn_modPow_bbb(b, e, m) {
  var r = h$ghcjsbn_one_b, b = h$ghcjsbn_rem_bb(b, m);
  while(!h$ghcjsbn_eq_bw(e, 0)) {
    if(h$ghcjsbn_testBit_b(e, 0)) {
      r = h$ghcjsbn_rem_bb(h$ghcjsbn_mul_bb(r, b), m);
    }
    e = h$ghcjsbn_shr_b(e, 1);
    b = h$ghcjsbn_rem_bb(h$ghcjsbn_mul_bb(b, b), m);
  }
  return r;
}
function h$ghcjsbn_modPow_bss(b, e, m) {
  return h$ghcjsbn_modPow_sss(h$ghcjsbn_rem_bw(b, m), e, m);
}
function h$ghcjsbn_modPow_sss(b, e, m) {
  return h$integer_gmp_powm_word(b, e, m);
}
function h$ghcjsbn_modular_inverse(a, n) {
    var t = h$ghcjsbn_zero_b, newt = h$ghcjsbn_one_b;
    var r = n, newr = a;
    while(!h$ghcjsbn_cmp_bw(newr, 0)) {
      var quotient = h$ghcjsbn_div_bb(r, newr);
      var tmp = newt;
      newt = h$ghcjsbn_sub_bb(t, h$ghcjsbn_mul_bb(quotient, newt));
      t = tmp;
      tmp = newr;
      newr = h$ghcjsbn_sub_bb(r, h$ghcjsbn_mul_bb(quotient, newr));
      r = tmp;
    }
    if(h$ghcjsbn_cmp_bw(r, 1) > 0) return a;
    return t;
}
function h$ghcjsbn_powModSBigNat(bpos, base, epos, exp, m) {
  var newBase = bpos ? base : h$ghcjsbn_sub_bb(m, base);
  var newExp = epos ? exp : h$ghcjsbn_modular_inverse(exp, m);
  return h$ghcjsbn_modPow_bbb(newBase, newExp, m);
}
function h$integer_gmp_powm_word(b, e, m) {
  var r = 1, b = b % m;
  while(e !== 0) {
    if(e % 2 === 1) r = (r * b) % m;
    e = e >>> 1;
    b = (b * b) % m;
  }
  return r;
}
function h$ghcjsbn_gcd_bb(b1, b2) {
  ;
  ;
  var r;
  if(h$ghcjsbn_cmp_bb(b1, b2) === 2) {
    r = b1;
    b1 = b2;
    b2 = r;
  }
  while(b1[0] > 0) {
    r = h$ghcjsbn_rem_bb(b2, b1);
    b2 = b1;
    b1 = r;
  }
  ;
  return b2;
}
function h$ghcjsbn_gcd_bs(b, s) {
  throw new Error("h$ghcjsbn_gcd_bs not implemented");
}
function h$ghcjsbn_gcd_ss(s1, s2) {
  ;
  ;
  var a, b, r;
  a = s1 < 0 ? -s1 : s1;
  b = s2 < 0 ? -s2 : s2;
  if(b < a) {
    r = a;
    a = b;
    b = r;
  }
  while(a !== 0) {
    r = b % a;
    b = a;
    a = r;
  }
  ;
  return b;
}
function h$ghcjsbn_gcd_ww(w1, w2) {
  ;
  ;
  var a, b, r;
  a = w1 < 0 ? (w1 + 4294967296) : w1;
  b = w2 < 0 ? (w2 + 4294967296) : w2;
  if(b < a) {
    r = a;
    a = b;
    b = r;
  }
  while(a !== 0) {
    r = b % a;
    b = a;
    a = r;
  }
  b = b|0;
  ;
  return b;
}
function h$ghcjsbn_gcd_bw(b, w) {
  ;
  ;
  var q = [], r = h$ghcjsbn_quotRem_bw(q, b, w);
  ;
  if(r === 0) {
    return b[0] === 0 ? 0 : w;
  } else {
    return h$ghcjsbn_gcd_ww(r, w);
  }
}
function h$ghcjsbn_shr_b(b, s) {
  ;
  ;
  var i, v1, v2, l = b[0], sl = (s / 28)|0, t = [0];
  l -= sl;
  if(l <= 0) {
    t[0] = 0;
  } else {
    var sb1 = s % 28, sb2 = 28 - sb1, m = (1<<sb1)-1;
    var c = b[sl + 1] >> sb1, v;
    for(i = 1; i < l; i++) {
      v = b[i + sl + 1];
      t[i] = ((v&m) << sb2)|c;
      c = v >> sb1;
    }
    if(c !== 0) {
      t[l] = c;
      t[0] = l;
    } else {
      t[0] = l - 1;
    }
  }
  ;
  return t;
}
function h$ghcjsbn_shrTo_b(t, b, s) {
  ;
  ;
  var i, v1, v2, l = b[0], sl = (s / 28)|0;
  t[0] = 0;
  l -= sl;
  if(l <= 0) {
    t[0] = 0;
  } else {
    var sb1 = s % 28, sb2 = 28 - sb1, m = (1<<sb1)-1;
    var c = b[sl + 1] >> sb1, v;
    for(i = 1; i < l; i++) {
      v = b[i + sl + 1];
      t[i] = ((v&m) << sb2)|c;
      c = v >> sb1;
    }
    if(c !== 0) {
      t[l] = c;
      t[0] = l;
    } else {
      t[0] = l - 1;
    }
  }
  ;
}
function h$ghcjsbn_shr_neg_b(b, s) {
  if(s === 0) return b;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_zero_b) === 1) return b;
  var t = h$ghcjsbn_sub_bw(b, 1);
  t = h$ghcjsbn_shr_b(t, s);
  return h$ghcjsbn_add_bw(t, 1);
}
function h$ghcjsbn_shl_b(b, s) {
  ;
  ;
  var sl = (s / 28)|0;
  var sb1 = s % 28, sb2 = 28 - sb1;
  var l = b[0];
  if(l === 0) return h$ghcjsbn_zero_b;
  var c = 0, i, v, m = (1 <<sb1) - 1, t = [0];
  for(i = 1; i <= sl; i++) {
    t[i] = 0;
  }
  for(i = 1; i <= l; i++) {
    v = b[i];
    t[i + sl] = ((v << sb1) & 0xfffffff) | c;
    c = v >> sb2;
  }
  if(c !== 0) {
    t[l+sl+1] = c;
    t[0] = l + sl + 1;
  } else {
    t[0] = l + sl;
  }
  ;
  return t;
}
function h$ghcjsbn_shlTo_b(t, b, s) {
  ;
  ;
  var sl = (s / 28)|0;
  var sb1 = s % 28, sb2 = 28 - sb1;
  var l = b[0], c = 0, i, v, m = (1 <<sb1) - 1;
  t[0] = 0;
  for(i = 1; i <= sl; i++) {
    t[i] = 0;
  }
  for(i = 1; i <= l; i++) {
    v = b[i];
    t[i + sl] = ((v << sb1) & 0xfffffff) | c;
    c = v >> sb2;
  }
  if(c !== 0) {
    t[l+sl+1] = c;
    t[0] = l + sl + 1;
  } else {
    t[0] = l + sl;
  }
  ;
}
function h$ghcjsbn_shrTo_limbs_b(t, b, s) {
  ;
  ;
  var l = b[0], l1 = l - s, i;
  if(l1 < 1) {
    t[0] = 0;
  } else {
    t[0] = l1;
    for(i = 1; i <= l1; i++) t[i] = b[i+s];
  }
  ;
}
function h$ghcjsbn_shlTo_limbs_b(t, b, s) {
  ;
  ;
  var l = b[0], l1 = l + s, i;
  if(l === 0) {
    t[0] = 0;
  } else {
    t[0] = l1;
    for(i = 1; i <= s; i++) t[i] = 0;
    for(i = s+1; i <= l1; i++) t[i] = b[i-s];
  }
  ;
}
function h$ghcjsbn_nbits_b(b) {
  ;
  var l = b[0], c = 0, s, t;
  if(l === 0) {
    return 0;
  } else {
    var r = ((l-1)*28) + h$ghcjsbn_nbits_s(b[l]);
    ;
    return r;
  }
}
function h$ghcjsbn_nbits_s(s) {
  ;
  var c = 1, t;
  if((t = s >>> 16) != 0) { s = t; c += 16; }
  if((t = s >> 8) != 0) { s = t; c += 8; }
  if((t = s >> 4) != 0) { s = t; c += 4; }
  if((t = s >> 2) != 0) { s = t; c += 2; }
  if((t = s >> 1) != 0) { s = t; c += 1; }
  ;
  return c;
}
function h$ghcjsbn_showBase(b, base) {
  ;
  ;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_zero_b) === 1) {
    return "0";
  } else {
    return h$ghcjsbn_showBase_rec(b, base, Math.log(base), 0);
  }
}
function h$ghcjsbn_showBase_rec(b, base, logBase, pad) {
  var bits = h$ghcjsbn_nbits_b(b), r;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b) === 0) {
    var ti = h$ghcjsbn_toInt_b(b);
    r = ti === 0 ? "" : ti.toString(base);
  } else {
    var digits = Math.floor(bits * 0.6931471805599453 / logBase);
    var d2 = Math.round(digits/2), p, q = [], r = [];
    p = h$ghcjsbn_pow_ww(base, d2);
    h$ghcjsbn_quotRem_bb(q, r, b, p);
    r = h$ghcjsbn_showBase_rec(q, base, logBase, 0) +
        h$ghcjsbn_showBase_rec(r, base, logBase, d2);
  }
  var rl = r.length;
  if(rl < pad) {
    while(rl <= pad-8) { r = "00000000" + r; rl += 8; }
    switch(pad-rl) {
    case 1: r = "0" + r; break;
    case 2: r = "00" + r; break;
    case 3: r = "000" + r; break;
    case 4: r = "0000" + r; break;
    case 5: r = "00000" + r; break;
    case 6: r = "000000" + r; break;
    case 7: r = "0000000" + r; break;
    }
  }
  return r;
}
function h$ghcjsbn_show(b) {
  throw new Error("show not implemented");
}
function h$ghcjsbn_showHex(b) {
  throw new Error("showHex not implemented");
}
function h$ghcjsbn_copy(t, b) {
  ;
  var l = b[0];
  for(var i = 0; i <= l; i++) {
    t[i] = b[i];
  }
  return l;
}
function h$ghcjsbn_testBit_b(b, n) {
  ;
  ;
  var limb = (n / 28)|0;
  if(limb >= b[0]) {
    return false;
  } else {
    var bit = n - (28 * limb);
    return (b[limb+1] & (1 << bit)) !== 0;
  }
}
function h$ghcjsbn_popCount_b(b) {
  ;
  var c = 0, l = b[0];
  while(l > 0) {
    c += h$popCnt32(b[l--]);
  }
  return c;
}
function h$ghcjsbn_xor_bb(b1, b2) {
  ;
  ;
  var i, lmin, lmax, blmax, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    blmax = b2;
  } else {
    lmin = l2;
    lmax = l1;
    blmax = b1;
  }
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] ^ b2[i];
  }
  for(i = lmin + 1; i <= lmax; i++) {
    t[i] = blmax[i];
  }
  while(lmax > 0 && t[lmax] === 0) lmax--;
  t[0] = lmax;
  ;
  return t;
}
function h$ghcjsbn_or_bb(b1, b2) {
  ;
  ;
  var i, lmin, lmax, blmax, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    blmax = b2;
  } else {
    lmin = l2;
    lmax = l1;
    blmax = b1;
  }
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] | b2[i];
  }
  for(i = lmin + 1; i <= lmax; i++) {
    t[i] = blmax[i];
  }
  t[0] = lmax;
  ;
  return t;
}
function h$ghcjsbn_and_bb(b1, b2) {
  ;
  ;
  var i, lmin, l1 = b1[0], l2 = b2[0], t = [0];
  lmin = l1 <= l2 ? l1 : l2;
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] & b2[i];
  }
  while(lmin > 0 && t[lmin] === 0) lmin--;
  t[0] = lmin;
  ;
  return t;
}
function h$ghcjsbn_andn_bb(b1, b2) {
  ;
  ;
  var i, lmin, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    for(i = 0; i <= l1; i++) t[i] = b1[i] & (~b2[i]);
  } else {
    for(i = 0; i <= l2; i++) t[i] = b1[i] & (~b2[i]);
    for(i = l2+1; i <= l1; i++) t[i] = b1[i];
  }
  while(l1 > 0 && t[l1] === 0) l1--;
  t[0] = l1;
  ;
  return t;
}
function h$ghcjsbn_toInt_b(b) {
  ;
  var bl = b[0], r;
  if(bl >= 2) {
    r = (b[2] << 28) | b[1];
  } else if(bl === 1) {
    r = b[1];
  } else {
    r = 0;
  }
  ;
  return r;
}
function h$ghcjsbn_toWord_b(b) {
  ;
  var bl = b[0], w;
  if(bl >= 2) {
    w = (b[2] << 28) | b[1];
  } else if(bl === 1) {
    w = b[1];
  } else {
    w = 0;
  }
  ;
  return w;
}
var h$integer_bigNatToWord64 = h$ghcjsbn_toWord64_b;
var h$integer_word64ToBigNat = h$ghcjsbn_mkBigNat_ww;
function h$ghcjsbn_toWord64_b(b) {
  ;
  var len = b[0], w1, w2;
  if(len < 2) {
    w2 = 0;
    w1 = (len === 1) ? b[1] : 0;
  } else {
    w1 = b[1] | (b[2] << 28);
    if(len === 2) {
      w2 = b[2] >>> 4;
    } else {
      w2 = (b[2] >>> 4) | (b[3] << 24);
    }
  }
  ;
  ;
  { h$ret1 = (w1); return (w2); };
}
function h$ghcjsbn_toBigNat_s(b, s) {
  ;
  if(s === 0) {
    b[0] = 0;
  } else if(s <= 0xfffffff) {
    b[0] = 1;
    b[1] = s;
  } else {
    b[0] = 2;
    b[1] = s & 0xfffffff;
    b[2] = s >> 0xfffffff;
  }
  ;
}
function h$ghcjsbn_toBigNat_w(b, w) {
  ;
  if(w === 0) {
    b[0] = 0;
  } else if(w > 0 && w <= 0xfffffff) {
    b[0] = 1;
    b[1] = w;
  } else {
    b[0] = 2;
    b[1] = w & 0xfffffff;
    b[2] = w >>> 28;
  }
  ;
}
function h$ghcjsbn_mkBigNat_w(w) {
  ;
  var r;
  if(w === 0) r = h$ghcjsbn_zero_b;
  else if(w === 1) r = h$ghcjsbn_one_b;
  else if(w > 0 && w <= 0xfffffff) r = [1,w];
  else r = [2, w & 0xfffffff, w >>> 28];
  ;
  return r;
}
function h$ghcjsbn_mkBigNat_ww(hw, lw) {
  ;
  ;
  var r;
  if(hw === 0) r = h$ghcjsbn_mkBigNat_w(lw);
  else {
    var w1 = lw & 0xfffffff;
    var w2 = (lw >>> 28) | ((hw << 4) & 0xfffffff);
    var w3 = hw >>> 24;
    if(w3 === 0) {
      r = [2, w1, w2];
    } else {
      r = [3, w1, w2, w3];
    }
  }
  ;
  return r;
}
var h$ghcjsbn_toBigNat_ww = h$ghcjsbn_mkBigNat_ww;
var h$integer_mkInteger = h$ghcjsbn_mkInteger;
function h$ghcjsbn_mkInteger(nonNeg, xs) {
  var r = [0], s = 0, t;
  while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
    t = h$ghcjsbn_shl_b(h$ghcjsbn_mkBigNat_w(((typeof(((xs).d1)) === 'number')?(((xs).d1)):(((xs).d1)).d1)), s);
    h$ghcjsbn_addTo_bb(r, t);
    s += 31;
    xs = ((xs).d2);
  }
  if(nonNeg) {
    if(h$ghcjsbn_cmp_bb(r, h$ghcjsbn_two31_b) === 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (h$ghcjsbn_toInt_b(r))));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (r)));;
    }
  } else {
    var c = h$ghcjsbn_cmp_bb(r, h$ghcjsbn_two31_b);
    if(c === 2) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (r)));;
    } else if(c === 1) {
      return h$ghcjsbn_negTwo31_i;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-h$ghcjsbn_toInt_b(r))));;
    }
  }
}
function h$ghcjsbn_cmp_bw(b, w) {
  ;
  ;
  var w1 = w & 0xfffffff, w2 = w >>> 28, bl = b[0];
  if(w2 === 0) {
    if(bl === 0) {
      return w1 > 0 ? 0 : 1;
    } else if(bl === 1) {
      var bw = b[1];
      return bw > w1 ? 2 : (bw === w1 ? 1 : 0);
    } else {
      return 2;
    }
  } else {
    if(bl < 2) {
      return 0;
    } else if(bl > 2) {
      return 2;
    } else {
      var bw1 = b[1], bw2 = b[2];
      return (bw2 > w2) ? 2
                        : (bw2 < w2 ? 0
                                    : (bw1 > w1 ? 2
                                                : (bw1 < w1 ? 0
                                                            : 1)));
    }
  }
}
function h$ghcjsbn_gt_bw(b, w) {
  ;
  ;
  var bl = b[0];
  if(bl > 2) return true;
  else if(bl === 0) return false;
  else if(bl === 1) return w >= 0 && b[1] > w;
  else {
    var wh = w >>> 28, wl = w & 0xfffffff, b2 = b[2];
    return (b2 > wh || ((wh === b2) && b[1] > wl));
  }
}
function h$ghcjsbn_eq_bb(b1, b2) {
  ;
  ;
  var bl1 = b1[0], bl2 = b2[0];
  if(bl1 !== bl2) {
    return false;
  } else {
    for(var i = bl1; i >= 1; i--) {
      var bw1 = b1[i], bw2 = b2[i];
      if(bw1 !== bw2) return false;
    }
  }
  return true;
}
function h$ghcjsbn_neq_bb(b1, b2) {
  ;
  ;
  var bl1 = b1[0], bl2 = b2[0];
  if(bl1 !== bl2) {
    return true;
  } else {
    for(var i = bl1; i >= 1; i--) {
      var bw1 = b1[i], bw2 = b2[i];
      if(bw1 !== bw2) return true;
    }
  }
  return false;
}
function h$ghcjsbn_eq_bw(b, w) {
  ;
  ;
  var w1 = w & 0xfffffff, w2 = w >>> 28, bl = b[0];
  if(w2 === 0) {
    if(w1 === 0) {
      return bl === 0;
    } else {
      return bl === 1 && b[1] === w;
    }
  } else {
    return bl === 2 && b[1] === w1 && b[2] === w2;
  }
}
function h$ghcjsbn_isZero_b(b) {
  ;
  return b[0] === 0;
}
function h$ghcjsbn_isNull_b(b) {
  return b[0] === -1;
}
function h$ghcjsbn_bitBigNat(n) {
  if(n === 0) {
    r = h$ghcjsbn_one_b;
  } else if(n < 28) {
    r = [1, 1 << n];
  } else {
    var l = (n / 28)|0;
    var r = [l+1];
    for(var i = 1; i<= l; i++) r[i] = 0;
    r[l+1] = 1 << (n - (28 * l));
  }
  ;
  return r;
}
function h$ghcjsbn_integerLog2(i) {
  ;
  if(((i).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    return h$ghcjsbn_nbits_s(((i).d1))-1;
  } else {
    return h$ghcjsbn_nbits_b(((i).d1))-1;
  }
}
function h$ghcjsbn_integerLog2IsPowerOf2(i) {
  ;
  var nb;
  if(((i).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    var sd = ((i).d1);
    ;
    nb = h$ghcjsbn_nbits_s(sd);
    return ((sd === 1 << nb) ? -nb : nb);
  } else {
    var bd = ((i).d1);
    ;
    nb = h$ghcjsbn_nbits_b(bd);
    var i, bl = (nb / 28) | 0, lb = nb - 28 * bl, l = bd[bl+1];
    if(l !== (1 << lb)) return nb;
    for(i = bl; i >= 1; i--) {
      if(bd[i] !== 0) return nb;
    }
    return -nb;
  }
}
function h$ghcjsbn_isValid_b(b) {
  if(!Array.isArray(b)) return 0;
  if(b.length < 1) return 0;
  var bl = b[0], w;
  if(b.length < (bl+1)) return 0;
  for(var i = 0; i <= bl; i++) {
    w = b[i];
    if(typeof w !== 'number' || (w & 0xfffffff) !== w) return 0;
  }
  return 1;
}
function h$ghcjsbn_toInteger_b(b) {
  ;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b) === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (h$ghcjsbn_toInt_b(b))));;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (b)));;
  }
}
function h$ghcjsbn_toNegInteger_b(b) {
  ;
  var c = h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b);
  if(c === 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (-h$ghcjsbn_toInt_b(b))));;
  } else if(c === 1) {
    return h$ghcjsbn_negTwo31_i;
  } else {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (b)));;
  }
}
function h$ghcjsbn_sizeof_b(b) {
  if(b.length < 1) return 0;
  var bl = b[0];
  return Math.ceil((bl * 28) / 32);
}
function h$ghcjsbn_index_b(b, w) {
  var limb = (32 * w / 28)|0;
  var l = b[0];
  var l1 = limb < l ? b[limb+1] : 0;
  var l2 = (limb+1) < l ? b[limb+2] : 0;
  var ws = 4 * (w % 7);
  return (l1 >>> ws) | (l2 << 28 - ws);
}
function h$ghcjsbn_toDouble_b(nonNeg, b) {
  throw new Error("toDouble_b");
}
function h$ghcjsbn_byteArrayToBigNat(ba, len) {
  return h$ghcjsbn_importBigNatFromByteArray(ba, 0, len, 0);
}
function h$ghcjsbn_importBigNatFromAddr(a_d, a_o, len, msbf) {
  var r = h$ghcjsbn_zero_b;
  for(var i=0;i<len;i++) {
    var off = msbf ? i : (len-i-1);
    var val = a_d.u8[off];
    var r = h$ghcjsbn_or_bb(h$ghcjsbn_shl_b(r, 8), h$ghcjsbn_mkBigNat_w(val));
  }
  return r;
}
function h$ghcjsbn_importBigNatFromByteArray(ba, ofs, len, msbf) {
  return h$ghcjsbn_importBigNatFromAddr(ba, ofs, len, msbf);
}
function h$ghcjsbn_exportToAddr_b(bn, a_d, a_o, msbf) {
  var bytes = h$ghcjsbn_sizeInBase_b(bn, 256);
  for(var i=0;i<bytes;i++) {
    var b = h$ghcjsbn_toWord_b(bn) & 255;
    var off = msbf ? bytes-i-1 : i;
    bn = h$ghcjsbn_shr_b(bn, 8);
    a_d.u8[a_o+off] = b;
  }
  return bytes;
}
function h$ghcjsbn_exportToAddr_w(w, a_d, a_o, msbf) {
  return h$ghcjsbn_exportToAddr_b(h$ghcjsbn_mkBigNat_w(w), a_d, a_o, msbf);
}
var h$integer_int64ToInteger = h$ghcjsbn_toInteger_s64;
function h$ghcjsbn_toInteger_s64(s_a, s_b) {
  ;
  ;
  if(s_a === 0) {
    if(s_b >= 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (s_b)));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (h$ghcjsbn_mkBigNat_w(s_b))));;
    }
  } else if(s_a === -1) {
    if(s_b < 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (s_b)));;
    } else if(s_b === 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_ww(1,0))));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_w(((~s_b)+1)|0))));;
    }
  } else if(s_a > 0) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e, (h$ghcjsbn_mkBigNat_ww(s_a, s_b))));;
  } else {
    if(s_b === 0) {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_ww(((~s_a)+1)|0, 0))));;
    } else {
      return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e, (h$ghcjsbn_mkBigNat_ww((~s_a)|0, ((~s_b)+1)|0))));;
    }
  }
}
function h$decodeDoubleInt64(d) {
  ;
  if(isNaN(d)) {
    { h$ret1 = (-1572864); h$ret2 = (0); return (972); };
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) {
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  { h$ret1 = (ret1); h$ret2 = (ret2); return (ret3); };
}
function h$primop_DoubleDecode_Int64Op(d) {
  ;
  if(isNaN(d)) {
    { h$ret1 = (-1572864); h$ret2 = (0); h$ret3 = (972); return (null); };
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) {
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  { h$ret1 = (ret1); h$ret2 = (ret2); h$ret3 = (ret3); return (null); };
}
function h$ghcjsbn_encodeDouble_b(pos, b, e) {
  ;
  ;
  if(e >= 972) {
    return pos ? Infinity : -Infinity;
  }
  var ls = 1, bl = b[0], i, r = b[bl], mul = 1 << 28, rmul = 1/mul, s = 1;
  for(i = bl-1; i >= 1; i--) {
      r = r * mul + s * b[i];
  }
  if(e > 600) {
    r = r * Math.pow(2, e-600) * Math.pow(2,600);
  } else if(e < -600) {
    r = r * Math.pow(2, e+600) * Math.pow(2,-600);
  } else {
    r = r * Math.pow(2, e);
  }
  ;
  return pos ? r : -r;
}
function h$ghcjsbn_toDouble_b(nonNeg, b) {
  return h$ghcjsbn_encodeDouble_b(nonNeg, b, 0);
}
var h$ghcjsbn_encodeDouble_i = h$ghcjsbn_encodeDouble_s;
function h$ghcjsbn_encodeDouble_s(m, e) {
  ;
  ;
  var r = m * Math.pow(2, e);
  ;
  return r;
}
function h$ghcjsbn_sizeInBase_b(bn, base) {
  if(h$ghcjsbn_eq_bb(bn, h$ghcjsbn_zero_b)) return 1;
  var bits = h$ghcjsbn_nbits_b(bn);
  var r;
  if(h$popCnt32(base) === 1) {
    var factor = Math.round(Math.log(base)/Math.log(2));
    r = Math.ceil(bits/factor);
  } else {
    r = Math.ceil(bits*Math.log(2)/Math.log(base));
  }
  return r;
}
function h$integer_gmp_mpn_sizeinbase1(w, base) {
  return h$ghcjsbn_sizeInBase_b(h$ghcjsbn_mkBigNat_w(w), base);
}
function Buffer(source, ctx) {
  this.source = source;
  this.ctx = ctx;
  this.status = 'created';
  this.error = null;
  this.buffer = null;
}
Buffer.prototype._changeStatus = function (onStatusChange, status, error, buffer) {
  this.status = status;
  this.error = error != null ? error : null;
  this.buffer = buffer != null ? buffer : null;
  onStatusChange(this);
}
Buffer.prototype.startLoadingAndDecoding = function(onStatusChange) {
  var closure = this;
  var sourceIsUrl = typeof this.source === 'string';
  this._changeStatus(onStatusChange, 'loading');
  var loader = sourceIsUrl ? new XMLHttpRequest() : new FileReader();
  loader.onerror = function () {
    var message = 'Error loading buffer: ' + (sourceIsUrl ? loader.statusText : loader.error.message);
    closure._changeStatus(onStatusChange, 'error', message);
  }
  loader.onabort = this._changeStatus.bind(this, onStatusChange, 'abort', 'Read aborted', null);
  loader.onload = function() {
    closure._changeStatus(onStatusChange, 'decoding');
    var arrayBuffer = sourceIsUrl ? loader.response : loader.result;
    closure.ctx.decodeAudioData(arrayBuffer,
      function success(audioBuffer) {
        closure._changeStatus(onStatusChange, 'decoded', null, audioBuffer);
      },
      function failure(error) {
        closure._changeStatus(onStatusChange, 'error', 'Error decoding buffer: ' + error.message);
      }
    );
  }
  if (sourceIsUrl) {
    loader.responseType = 'arraybuffer';
    loader.open('GET', this.source, true);
    loader.send();
  } else {
    loader.readAsArrayBuffer(this.source);
  }
}
function startLoadingAndDecodingMultiple(list, cb){
  var closure = []
  var loaded = []
  for (i in list){
    console.log(i)
    console.log(list[i])
    console.log(loaded)
    list[i].startLoadingAndDecoding((x)=>{
      console.log("x is : " + x)
      console.log("x.status is : " + x.status)
      closure.push(x)
      var haveLoaded = true
      for (j in closure){
        haveLoaded = (closure[j].status == "error" || closure[j].status == "abort" || closure[j].status == "decoded")&&haveLoaded;
        if(haveLoaded){
          loaded.push(closure[j])
        }
      }
      console.log("cb test case:  haveloaded - "+(haveLoaded) +"  loadedlength - "+loaded.length);
      if (haveLoaded && loaded.length >= list.length ){
        console.log("cb being called...")
        cb(closure)
      }
    });
  }
  console.log("closure:  "+closure)
}
function h$dom$sendXHR(xhr, d, cont) {
    var clear;
    var error = function () {
        clear(); cont(2);
    };
    var abort = function () {
        clear(); cont(1);
    };
    var load = function () {
        clear(); cont(0);
    };
    clear = function () {
        xhr.removeEventListener('error', error);
        xhr.removeEventListener('abort', abort);
        xhr.removeEventListener('load', load);
    }
    xhr.addEventListener('error', error);
    xhr.addEventListener('abort', abort);
    xhr.addEventListener('load', load);
    if(d) {
 xhr.send(d);
    } else {
 xhr.send();
    }
}
function h$fromArray(a) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=a.length-1;i>=0;i--) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return a;
}
function h$fromArrayNoWrap(a) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=a.length-1;i>=0;i--) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return a;
}
function h$listToArray(xs) {
    var a = [], i = 0;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 a[i++] = ((((xs).d1)).d1);
 xs = ((xs).d2);
    }
    return a;
}
function h$listToArrayWrap(xs) {
    return (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (h$listToArray(xs))));
}
function h$animationFrameCancel(h) {
    if(h.handle) window.cancelAnimationFrame(h.handle);
    if(h.callback) {
        h$release(h.callback)
        h.callback = null;
    }
}
function h$animationFrameRequest(h) {
    h.handle = window.requestAnimationFrame(function(ts) {
        var cb = h.callback;
        if(cb) {
         h$release(cb);
         h.callback = null;
         cb(ts);
        }
    });
}
function h$exportValue(fp1a,fp1b,fp2a,fp2b,o) {
  var e = { fp1a: fp1a
          , fp1b: fp1b
          , fp2a: fp2a
          , fp2b: fp2b
          , released: false
          , root: o
          , _key: -1
          };
  h$retain(e);
  return e;
}
function h$derefExport(fp1a,fp1b,fp2a,fp2b,e) {
  if(!e || typeof e !== 'object') return null;
  if(e.released) return null;
  if(fp1a !== e.fp1a || fp1b !== e.fp1b ||
     fp2a !== e.fp2a || fp2b !== e.fp2b) return null;
  return e.root;
}
function h$releaseExport(e) {
  h$release(e);
  e.released = true;
  e.root = null;
}
var h$jsstringEmpty = (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, ('')));
var h$jsstringHead, h$jsstringTail, h$jsstringCons,
    h$jsstringSingleton, h$jsstringSnoc, h$jsstringUncons,
    h$jsstringIndex, h$jsstringUncheckedIndex;
var h$fromCodePoint;
if(String.prototype.fromCodePoint) {
    h$fromCodePoint = String.fromCodePoint;
} else {
    h$fromCodePoint =
      (function() {
          var stringFromCharCode = String.fromCharCode;
          var floor = Math.floor;
          return function(_) {
              var MAX_SIZE = 0x4000;
              var codeUnits = [];
              var highSurrogate;
              var lowSurrogate;
              var index = -1;
              var length = arguments.length;
              if (!length) {
                  return '';
              }
              var result = '';
              while (++index < length) {
                  var codePoint = Number(arguments[index]);
                  if (
                      !isFinite(codePoint) ||
                      codePoint < 0 ||
                      codePoint > 0x10FFFF ||
                      floor(codePoint) != codePoint
                  ) {
                      throw RangeError('Invalid code point: ' + codePoint);
                  }
                  if (codePoint <= 0xFFFF) {
                      codeUnits.push(codePoint);
                  } else {
                      codePoint -= 0x10000;
                      highSurrogate = (codePoint >> 10) + 0xD800;
                      lowSurrogate = (codePoint % 0x400) + 0xDC00;
                      codeUnits.push(highSurrogate, lowSurrogate);
                  }
                  if (index + 1 == length || codeUnits.length > MAX_SIZE) {
                      result += stringFromCharCode.apply(null, codeUnits);
                      codeUnits.length = 0;
                  }
              }
              return result;
          }
      })();
}
if(String.prototype.codePointAt) {
    h$jsstringSingleton = function(ch) {
        ;
 return String.fromCodePoint(ch);
    }
    h$jsstringHead = function(str) {
        ;
 var cp = str.codePointAt(0);
 return (cp === undefined) ? -1 : (cp|0);
    }
    h$jsstringTail = function(str) {
        ;
 var l = str.length;
 if(l===0) return null;
 var ch = str.codePointAt(0);
 if(ch === undefined) return null;
 return str.substr(((ch)>=0x10000)?2:1);
    }
    h$jsstringCons = function(ch, str) {
        ;
 return String.fromCodePoint(ch)+str;
    }
    h$jsstringSnoc = function(str, ch) {
        ;
 return str+String.fromCodePoint(ch);
    }
    h$jsstringUncons = function(str) {
        ;
 var l = str.length;
 if(l===0) {
          { h$ret1 = (null); return (-1); };
        }
 var ch = str.codePointAt(0);
        if(ch === undefined) {
     { h$ret1 = (null); return (-1); };
        }
        { h$ret1 = (str.substr(((ch)>=0x10000)?2:1)); return (ch); };
    }
    h$jsstringIndex = function(i, str) {
        ;
 var ch = str.codePointAt(i);
 if(ch === undefined) return -1;
 return ch;
    }
    h$jsstringUncheckedIndex = function(i, str) {
        ;
 return str.codePointAt(i);
    }
} else {
    h$jsstringSingleton = function(ch) {
        ;
 return (((ch)>=0x10000)) ? String.fromCharCode(((((ch)-0x10000)>>>10)+0xDC00), (((ch)&0x3FF)+0xD800))
                               : String.fromCharCode(ch);
    }
    h$jsstringHead = function(str) {
        ;
 var l = str.length;
 if(l===0) return -1;
 var ch = str.charCodeAt(0);
 if(((ch|1023)===0xDBFF)) {
     return (l>1) ? ((((ch)-0xD800)<<10)+(str.charCodeAt(1))-0xDC00+0x10000) : -1;
 } else {
     return ch;
 }
    }
    h$jsstringTail = function(str) {
        ;
 var l = str.length;
 if(l===0) return null;
 var ch = str.charCodeAt(0);
 if(((ch|1023)===0xDBFF)) {
     return (l>1)?str.substr(2):null;
 } else return str.substr(1);
    }
    h$jsstringCons = function(ch, str) {
        ;
 return ((((ch)>=0x10000)) ? String.fromCharCode(((((ch)-0x10000)>>>10)+0xDC00), (((ch)&0x3FF)+0xD800))
                                : String.fromCharCode(ch))
                                + str;
    }
    h$jsstringSnoc = function(str, ch) {
        ;
 return str + ((((ch)>=0x10000)) ? String.fromCharCode(((((ch)-0x10000)>>>10)+0xDC00), (((ch)&0x3FF)+0xD800))
                                      : String.fromCharCode(ch));
    }
    h$jsstringUncons = function(str) {
        ;
 var l = str.length;
 if(l===0) {
          { h$ret1 = (null); return (-1); };
        }
 var ch = str.charCodeAt(0);
 if(((ch|1023)===0xDBFF)) {
   if(l > 1) {
        { h$ret1 = (str.substr(2)); return (((((ch)-0xD800)<<10)+(str.charCodeAt(1))-0xDC00+0x10000)); };
   } else {
       { h$ret1 = (null); return (-1); };
   }
 } else {
      { h$ret1 = (str.substr(1)); return (ch); };
 }
    }
    h$jsstringIndex = function(i, str) {
 var ch = str.charCodeAt(i);
 if(ch != ch) return -1;
 return (((ch|1023)===0xDBFF)) ? ((((ch)-0xD800)<<10)+(str.charCodeAt(i+1))-0xDC00+0x10000) : ch;
    }
    h$jsstringUncheckedIndex = function(i, str) {
        ;
 var ch = str.charCodeAt(i);
 return (((ch|1023)===0xDBFF)) ? ((((ch)-0xD800)<<10)+(str.charCodeAt(i+1))-0xDC00+0x10000) : ch;
    }
}
function h$jsstringUnsnoc(str) {
  ;
  var l = str.length;
  if(l===0) {
    { h$ret1 = (null); return (-1); };
  }
  var ch = str.charCodeAt(l-1);
  if(((ch|1023)===0xDFFF)) {
    if(l !== 1) {
      { h$ret1 = (str.substr(0,l-2)); return (((((str.charCodeAt(l-2))-0xD800)<<10)+(ch)-0xDC00+0x10000)); };
    } else {
      { h$ret1 = (null); return (-1); };
    }
  } else {
    { h$ret1 = (str.substr(0,l-1)); return (ch); };
  }
}
function h$jsstringPack(xs) {
    var r = '', i = 0, a = [], c;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 c = ((xs).d1);
 a[i++] = ((typeof(c) === 'number')?(c):(c).d1);
 if(i >= 60000) {
     r += h$fromCodePoint.apply(null, a);
     a = [];
     i = 0;
 }
 xs = ((xs).d2);
    }
    if(i > 0) r += h$fromCodePoint.apply(null, a);
    ;
    return r;
}
function h$jsstringPackReverse(xs) {
    var a = [], i = 0, c;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 c = ((xs).d1);
 a[i++] = ((typeof(c) === 'number')?(c):(c).d1);
 xs = ((xs).d2);
    }
    if(i===0) return '';
    var r = h$jsstringConvertArray(a.reverse());
    ;
    return r;
}
function h$jsstringPackArray(arr) {
    ;
    return h$jsstringConvertArray(arr);
}
function h$jsstringPackArrayReverse(arr) {
    ;
    return h$jsstringConvertArray(arr.reverse());
}
function h$jsstringConvertArray(arr) {
    if(arr.length < 60000) {
 return h$fromCodePoint.apply(null, arr);
    } else {
 var r = '';
 for(var i=0; i<arr.length; i+=60000) {
     r += h$fromCodePoint.apply(null, arr.slice(i, i+60000));
 }
 return r;
    }
}
function h$jsstringInit(str) {
    ;
    var l = str.length;
    if(l===0) return null;
    var ch = str.charCodeAt(l-1);
    var o = ((ch|1023)===0xDFFF)?2:1;
    var r = str.substr(0, l-o);
    return r;
}
function h$jsstringLast(str) {
    ;
    var l = str.length;
    if(l===0) return -1;
    var ch = str.charCodeAt(l-1);
    if(((ch|1023)===0xDFFF)) {
 return (l>1) ? ((((str.charCodeAt(l-2))-0xD800)<<10)+(ch)-0xDC00+0x10000) : -1;
    } else return ch;
}
function h$jsstringIndexR(i, str) {
    ;
    if(i < 0 || i > str.length) return -1;
    var ch = str.charCodeAt(i);
    return (((ch|1023)===0xDFFF)) ? ((((str.charCodeAt(i-1))-0xD800)<<10)+(ch)-0xDC00+0x10000) : ch;
}
function h$jsstringNextIndex(i, str) {
    ;
    return i + (((str.charCodeAt(i)|1023)===0xDBFF)?2:1);
}
function h$jsstringTake(n, str) {
    ;
    if(n <= 0) return '';
    var i = 0, l = str.length, ch;
    if(n >= l) return str;
    while(n--) {
 ch = str.charCodeAt(i++);
 if(((ch|1023)===0xDBFF)) i++;
 if(i >= l) return str;
    }
    return str.substr(0,i);
}
function h$jsstringDrop(n, str) {
    ;
    if(n <= 0) return str;
    var i = 0, l = str.length, ch;
    if(n >= l) return '';
    while(n--) {
 ch = str.charCodeAt(i++);
 if(((ch|1023)===0xDBFF)) i++;
 if(i >= l) return '';
    }
    return str.substr(i);
}
function h$jsstringSplitAt(n, str) {
  ;
  if(n <= 0) {
    { h$ret1 = (str); return (""); };
  } else if(n >= str.length) {
    { h$ret1 = (""); return (str); };
  }
  var i = 0, l = str.length, ch;
  while(n--) {
    ch = str.charCodeAt(i++);
    if(((ch|1023)===0xDBFF)) i++;
    if(i >= l) {
      { h$ret1 = (""); return (str); };
    }
  }
  { h$ret1 = (str.substr(i)); return (str.substr(0,i)); };
}
function h$jsstringTakeEnd(n, str) {
    ;
    if(n <= 0) return '';
    var l = str.length, i = l-1, ch;
    if(n >= l) return str;
    while(n-- && i > 0) {
 ch = str.charCodeAt(i--);
 if(((ch|1023)===0xDFFF)) i--;
    }
    return (i<0) ? str : str.substr(i+1);
}
function h$jsstringDropEnd(n, str) {
    ;
    if(n <= 0) return str;
    var l = str.length, i = l-1, ch;
    if(n >= l) return '';
    while(n-- && i > 0) {
 ch = str.charCodeAt(i--);
 if(((ch|1023)===0xDFFF)) i--;
    }
    return (i<0) ? '' : str.substr(0,i+1);
}
function h$jsstringIntercalate(x, ys) {
    ;
    var a = [], i = 0;
    while(((ys).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 if(i) a[i++] = x;
 a[i++] = ((((ys).d1)).d1);
 ys = ((ys).d2);
    }
    return a.join('');
}
function h$jsstringIntersperse(ch, ys) {
    ;
    var i = 0, l = ys.length, j = 0, a = [], ych;
    if(((ch)>=0x10000)) {
 while(j < l) {
     if(i) a[i++] = ch;
     ych = ys.charCodeAt(j++);
     a[i++] = ych;
     if(((ych|1023)===0xDBFF)) a[i++] = ys.charCodeAt(j++);
 }
    } else {
 while(j < l) {
     if(i) a[i++] = ch;
     ych = ys.charCodeAt(j++);
     a[i++] = ych;
     if(((ych|1023)===0xDBFF)) a[i++] = ys.charCodeAt(j++);
 }
    }
    return h$jsstringConvertArray(a);
}
function h$jsstringConcat(xs) {
    ;
    var a = [], i = 0;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 a[i++] = ((((xs).d1)).d1);
 xs = ((xs).d2);
    }
    return a.join('');
}
var h$jsstringStripPrefix, h$jsstringStripSuffix,
    h$jsstringIsPrefixOf, h$jsstringIsSuffixOf,
    h$jsstringIsInfixOf;
if(String.prototype.startsWith) {
    h$jsstringStripPrefix = function(p, x) {
 ;
 if(x.startsWith(p)) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(p.length)))))));
 } else {
     return h$baseZCGHCziMaybeziNothing;
 }
    }
    h$jsstringIsPrefixOf = function(p, x) {
 ;
 return x.startsWith(p);
    }
} else {
    h$jsstringStripPrefix = function(p, x) {
 ;
 if(x.indexOf(p) === 0) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(p.length)))))));
 } else {
   return h$baseZCGHCziMaybeziNothing;
 }
    }
    h$jsstringIsPrefixOf = function(p, x) {
 ;
 return x.indexOf(p) === 0;
    }
}
if(String.prototype.endsWith) {
    h$jsstringStripSuffix = function(s, x) {
 ;
 if(x.endsWith(s)) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,x.length-s.length)))))));
 } else {
   return h$baseZCGHCziMaybeziNothing;
 }
    }
    h$jsstringIsSuffixOf = function(s, x) {
 ;
 return x.endsWith(s);
    }
} else {
    h$jsstringStripSuffix = function(s, x) {
 ;
 var i = x.lastIndexOf(s);
 var l = x.length - s.length;
 if(i !== -1 && i === l) {
     return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,l)))))));
 } else {
   return h$baseZCGHCziMaybeziNothing;
 }
    }
      h$jsstringIsSuffixOf = function(s, x) {
 ;
        var i = x.lastIndexOf(s);
 return i !== -1 && i === x.length - s.length;
    }
}
if(String.prototype.includes) {
    h$jsstringIsInfixOf = function(i, x) {
        ;
 return x.includes(i);
    }
} else {
    h$jsstringIsInfixOf = function(i, x) {
        ;
 return x.indexOf(i) !== -1;
    }
}
function h$jsstringCommonPrefixes(x, y) {
    ;
    var lx = x.length, ly = y.length, i = 0, cx;
    var l = lx <= ly ? lx : ly;
    if(lx === 0 || ly === 0 || x.charCodeAt(0) !== y.charCodeAt(0)) {
      return h$baseZCGHCziMaybeziNothing;
    }
    while(++i<l) {
 cx = x.charCodeAt(i);
 if(cx !== y.charCodeAt(i)) {
     if(((cx|1023)===0xDFFF)) i--;
     break;
 }
    }
  if(i===0) return h$baseZCGHCziMaybeziNothing;
    return (h$c1(h$baseZCGHCziMaybeziJust_con_e, ((h$c3(h$ghczmprimZCGHCziTupleziZLz2cUz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, ((i===lx)?x:((i===ly)?y:x.substr(0,i)))))),((i===lx) ? h$jsstringEmpty : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(i))))),((i===ly) ? h$jsstringEmpty : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (y.substr(i))))))))));
}
function h$jsstringBreakOn(b, x) {
    ;
    var i = x.indexOf(b);
    if(i===-1) {
        { h$ret1 = (""); return (x); };
    }
    if(i===0) {
        { h$ret1 = (x); return (""); };
    }
    { h$ret1 = (x.substr(i)); return (x.substr(0,i)); };
}
function h$jsstringBreakOnEnd(b, x) {
    ;
    var i = x.lastIndexOf(b);
  if(i===-1) {
    { h$ret1 = (x); return (""); };
    }
  i += b.length;
    { h$ret1 = (x.substr(i)); return (x.substr(0,i)); };
}
function h$jsstringBreakOnAll1(n, b, x) {
    ;
    var i = x.indexOf(b, n);
    if(i===0) {
       { h$ret1 = (""); h$ret2 = (x); return (b.length); };
    }
    if(i===-1) {
       { h$ret1 = (null); h$ret2 = (null); return (-1); };
    }
    { h$ret1 = (x.substr(0,i)); h$ret2 = (x.substr(i)); return (i+b.length); };
}
function h$jsstringBreakOnAll(pat, src) {
    ;
    var a = [], i = 0, n = 0, r = h$ghczmprimZCGHCziTypesziZMZN, pl = pat.length;
    while(true) {
 var x = src.indexOf(pat, n);
 if(x === -1) break;
 a[i++] = (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (src.substr(0,x))))),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (src.substr(x)))))));
 n = x + pl;
    }
    while(--i >= 0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return r;
}
function h$jsstringSplitOn1(n, p, x) {
    ;
    var i = x.indexOf(p, n);
    if(i === -1) {
        { h$ret1 = (null); return (-1); };
    }
    var r1 = (i==n) ? "" : x.substr(n, i-n);
    { h$ret1 = (r1); return (i + p.length); };
}
function h$jsstringSplitOn(p, x) {
    ;
    var a = x.split(p);
    var r = h$ghczmprimZCGHCziTypesziZMZN, i = a.length;
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return r;
}
function h$jsstringWords1(n, x) {
    ;
    var m = n, s = n, l = x.length;
    if(m >= l) return -1;
    do {
 if(m >= l) return -1;
    } while(h$isSpace(x.charCodeAt(m++)));
    s = m - 1;
    while(m < l) {
 if(h$isSpace(x.charCodeAt(m++))) {
            var r1 = (m-s<=1) ? "" : x.substr(s,m-s-1);
            { h$ret1 = (r1); return (m); };
 }
    }
    if(s < l) {
        var r1 = s === 0 ? x : x.substr(s);
        { h$ret1 = (r1); return (m); };
    }
    { h$ret1 = (null); return (-1); };
}
function h$jsstringWords(x) {
    ;
    var a = null, i = 0, n, s = -1, m = 0, w, l = x.length, r = h$ghczmprimZCGHCziTypesziZMZN;
    outer:
    while(m < l) {
 do {
     if(m >= l) { s = m; break outer; }
 } while(h$isSpace(x.charCodeAt(m++)));
 s = m - 1;
 while(m < l) {
     if(h$isSpace(x.charCodeAt(m++))) {
  w = (m-s<=1) ? h$jsstringEmpty
                             : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(s,m-s-1))));
  if(i) a[i++] = w; else { a = [w]; i = 1; }
  s = m;
  break;
     }
 }
    }
    if(s !== -1 && s < l) {
 w = (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (s === 0 ? x : x.substr(s))));
 if(i) a[i++] = w; else { a = [w]; i = 1; }
    }
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return r;
}
function h$jsstringLines1(n, x) {
    ;
    var m = n, l = x.length;
    if(n >= l) return -1;
    while(m < l) {
 if(x.charCodeAt(m++) === 10) {
     if(n > 0 && n === l-1) return -1;
            var r1 = (m-n<=1) ? "" : x.substr(n,m-n-1);
            { h$ret1 = (r1); return (m); };
 }
    }
    { h$ret1 = (x.substr(n)); return (m); };
}
function h$jsstringLines(x) {
    ;
    var a = null, m = 0, i = 0, l = x.length, s = 0, r = h$ghczmprimZCGHCziTypesziZMZN, w;
    if(l === 0) return h$ghczmprimZCGHCziTypesziZMZN;
    outer:
    while(true) {
 s = m;
 do {
     if(m >= l) break outer;
 } while(x.charCodeAt(m++) !== 10);
 w = (m-s<=1) ? h$jsstringEmpty : (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(s,m-s-1))));
 if(i) a[i++] = w; else { a = [w]; i = 1; }
    }
    if(s < l) {
 w = (h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(s))));
 if(i) a[i++] = w; else { a = [w]; i = 1; }
    }
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (a[i]), (r)));
    return r;
}
function h$jsstringGroup(x) {
    ;
    var xl = x.length;
    if(xl === 0) return h$ghczmprimZCGHCziTypesziZMZN;
    var i = xl-1, si, ch, s=xl, r=h$ghczmprimZCGHCziTypesziZMZN;
    var tch = x.charCodeAt(i--);
    if(((tch|1023)===0xDFFF)) tch = ((((x.charCodeAt(i--))-0xD800)<<10)+(tch)-0xDC00+0x10000);
    while(i >= 0) {
 si = i;
 ch = x.charCodeAt(i--);
 if(((ch|1023)===0xDFFF)) {
     ch = ((((x.charCodeAt(i--))-0xD800)<<10)+(ch)-0xDC00+0x10000);
 }
 if(ch != tch) {
     tch = ch;
     r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(si+1,s-si))))), (r)));
     s = si;
 }
    }
    return (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,s+1))))), (r)));
}
function h$jsstringChunksOf1(n, s, x) {
    ;
    var m = s, c = 0, l = x.length, ch;
    if(n <= 0 || l === 0 || s >= l) return -1
    while(++m < l) {
        ch = x.charCodeAt(m - 1);
        if(((ch|1023)===0xDBFF)) ++m;
        if(++c >= n) break;
    }
    var r1 = (m >= l && s === c) ? x : x.substr(s,m-s);
    { h$ret1 = (r1); return (m); };
}
function h$jsstringChunksOf(n, x) {
    ;
    var l = x.length;
    if(l===0 || n <= 0) return h$ghczmprimZCGHCziTypesziZMZN;
    if(l <= n) return (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x)))), (h$ghczmprimZCGHCziTypesziZMZN)));
    var a = [], i = 0, s = 0, ch, m = 0, c, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(m < l) {
 s = m;
 c = 0;
 while(m < l && ++c <= n) {
     ch = x.charCodeAt(m++);
     if(((ch|1023)===0xDBFF)) ++m;
 }
 if(c) a[i++] = x.substr(s, m-s);
    }
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return r;
}
function h$jsstringCount(pat, src) {
    ;
    var i = 0, n = 0, pl = pat.length, sl = src.length;
    while(i<sl) {
 i = src.indexOf(pat, i);
 if(i===-1) break;
 n++;
 i += pl;
    }
    return n;
}
function h$jsstringReplicate(n, str) {
    ;
    if(n === 0 || str == '') return '';
    if(n === 1) return str;
    var r = '';
    do {
 if(n&1) r+=str;
        str+=str;
        n >>= 1;
    } while(n > 1);
    return r+str;
}
var h$jsstringReverse;
if(Array.from) {
    h$jsstringReverse = function(str) {
 ;
 return Array.from(str).reverse().join('');
    }
} else {
    h$jsstringReverse = function(str) {
 ;
 var l = str.length, a = [], o = 0, i = 0, c, c1, s = '';
 while(i < l) {
     c = str.charCodeAt(i);
     if(((c|1023)===0xDBFF)) {
  a[i] = str.charCodeAt(i+1);
  a[i+1] = c;
  i += 2;
     } else a[i++] = c;
     if(i-o > 60000) {
  s = String.fromCharCode.apply(null, a.reverse()) + s;
  o = -i;
  a = [];
     }
 }
 return (i===0) ? s : String.fromCharCode.apply(null,a.reverse()) + s;
    }
}
function h$jsstringUnpack(str) {
    ;
    var r = h$ghczmprimZCGHCziTypesziZMZN, i = str.length-1, c;
    while(i >= 0) {
 c = str.charCodeAt(i--);
 if(((c|1023)===0xDFFF)) c = ((((str.charCodeAt(i--))-0xD800)<<10)+(c)-0xDC00+0x10000)
 r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (c), (r)));
    }
    return r;
}
function h$jsstringDecInteger(val) {
  ;
  if(((val).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    return '' + ((val).d1);
  } else if(((val).f === h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e)) {
    return h$ghcjsbn_showBase(((val).d1), 10);
  } else {
    return '-' + h$ghcjsbn_showBase(((val).d1), 10);
  }
}
function h$jsstringDecI64(hi,lo) {
    ;
    var lo0 = (lo < 0) ? lo+4294967296:lo;
    if(hi < 0) {
 if(hi === -1) return ''+(lo0-4294967296);
 lo0 = 4294967296 - lo0;
 var hi0 = -1 - hi;
 var x0 = hi0 * 967296;
 var x1 = (lo0 + x0) % 1000000;
 var x2 = hi0*4294+Math.floor((x0+lo0-x1)/1000000);
 return '-' + x2 + h$jsstringDecIPadded6(x1);
    } else {
 if(hi === 0) return ''+lo0;
 var x0 = hi * 967296;
 var x1 = (lo0 + x0) % 1000000;
 var x2 = hi*4294+Math.floor((x0+lo0-x1)/1000000);
 return '' + x2 + h$jsstringDecIPadded6(x1);
    }
}
function h$jsstringDecW64(hi,lo) {
    ;
    var lo0 = (lo < 0) ? lo+4294967296 : lo;
    if(hi === 0) return ''+lo0;
    var hi0 = (hi < 0) ? hi+4294967296 : hi;
    var x0 = hi0 * 967296;
    var x1 = (lo0 + x0) % 1000000;
    var x2 = hi0*4294+Math.floor((x0+lo0-x1)/1000000);
    return '' + x2 + h$jsstringDecIPadded6(x1);
}
function h$jsstringHexInteger(val) {
  ;
  if(((val).f === h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e)) {
    return '' + ((val).d1);
  } else {
    return h$ghcjsbn_showBase(((val).d1), 16);
  }
}
function h$jsstringHexI64(hi,lo) {
    var lo0 = lo<0 ? lo+4294967296 : lo;
    if(hi === 0) return lo0.toString(16);
    return ((hi<0)?hi+4294967296:hi).toString(16) + h$jsstringHexIPadded8(lo0);
}
function h$jsstringHexW64(hi,lo) {
    var lo0 = lo<0 ? lo+4294967296 : lo;
    if(hi === 0) return lo0.toString(16);
    return ((hi<0)?hi+4294967296:hi).toString(16) + h$jsstringHexIPadded8(lo0);
}
function h$jsstringDecIPadded9(n) {
    ;
    if(n === 0) return '000000000';
    var pad = (n>=100000000)?'':
              (n>=10000000)?'0':
              (n>=1000000)?'00':
              (n>=100000)?'000':
              (n>=10000)?'0000':
              (n>=1000)?'00000':
              (n>=100)?'000000':
              (n>=10)?'0000000':
                     '00000000';
    return pad+n;
}
function h$jsstringDecIPadded6(n) {
    ;
    if(n === 0) return '000000';
    var pad = (n>=100000)?'':
              (n>=10000)?'0':
              (n>=1000)?'00':
              (n>=100)?'000':
              (n>=10)?'0000':
                     '00000';
    return pad+n;
}
function h$jsstringHexIPadded8(n) {
    ;
   if(n === 0) return '00000000';
   var pad = (n>=0x10000000)?'':
             (n>=0x1000000)?'0':
             (n>=0x100000)?'00':
             (n>=0x10000)?'000':
             (n>=0x1000)?'0000':
             (n>=0x100)?'00000':
             (n>=0x10)?'000000':
                      '0000000';
    return pad+n.toString(16);
}
function h$jsstringZeroes(n) {
    var r;
    switch(n&7) {
 case 0: r = ''; break;
 case 1: r = '0'; break;
 case 2: r = '00'; break;
 case 3: r = '000'; break;
 case 4: r = '0000'; break;
 case 5: r = '00000'; break;
 case 6: r = '000000'; break;
 case 7: r = '0000000';
    }
    for(var i=n>>3;i>0;i--) r = r + '00000000';
    return r;
}
function h$jsstringDoubleToFixed(decs, d) {
    if(decs >= 0) {
 if(Math.abs(d) < 1e21) {
     var r = d.toFixed(Math.min(20,decs));
     if(decs > 20) r = r + h$jsstringZeroes(decs-20);
     return r;
 } else {
     var r = d.toExponential();
     var ei = r.indexOf('e');
     var di = r.indexOf('.');
     var e = parseInt(r.substr(ei+1));
     return r.substring(0,di) + r.substring(di,ei) + h$jsstringZeroes(di-ei+e) +
                   ((decs > 0) ? ('.' + h$jsstringZeroes(decs)) : '');
 }
    }
    var r = Math.abs(d).toExponential();
    var ei = r.indexOf('e');
    var e = parseInt(r.substr(ei+1));
    var m = d < 0 ? '-' : '';
    r = r.substr(0,1) + r.substring(2,ei);
    if(e >= 0) {
 return (e > r.length) ? m + r + h$jsstringZeroes(r.length-e-1) + '.0'
                       : m + r.substr(0,e+1) + '.' + r.substr(e+1);
    } else {
 return m + '0.' + h$jsstringZeroes(-e-1) + r;
    }
}
function h$jsstringDoubleToExponent(decs, d) {
    var r;
    if(decs ===-1) {
 r = d.toExponential().replace('+','');
    } else {
 r = d.toExponential(Math.max(1, Math.min(20,decs))).replace('+','');
    }
    if(r.indexOf('.') === -1) {
 r = r.replace('e', '.0e');
    }
    if(decs > 20) r = r.replace('e', h$jsstringZeroes(decs-20)+'e');
    return r;
}
function h$jsstringDoubleGeneric(decs, d) {
    var r;
    if(decs === -1) {
 r = d.toString(10).replace('+','');
    } else {
 r = d.toPrecision(Math.max(decs+1,1)).replace('+','');
    }
    if(decs !== 0 && r.indexOf('.') === -1) {
 if(r.indexOf('e') !== -1) {
     r = r.replace('e', '.0e');
 } else {
     r = r + '.0';
 }
    }
    return r;
}
function h$jsstringAppend(x, y) {
    ;
    return x+y;
}
function h$jsstringCompare(x, y) {
    ;
    return (x<y)?-1:((x>y)?1:0);
}
function h$jsstringUnlines(xs) {
    var r = '';
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 r = r + ((((xs).d1)).d1) + '\n';
 xs = ((xs).d2);
    }
    return r;
}
function h$jsstringUnwords(xs) {
    if(((xs).f === h$ghczmprimZCGHCziTypesziZMZN_con_e)) return '';
    var r = ((((xs).d1)).d1);
    xs = ((xs).d2);
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 r = r + ' ' + ((((xs).d1)).d1);
 xs = ((xs).d2);
    }
    return r;
}
function h$jsstringReplace(pat, rep, src) {
    ;
    var r = src.replace(pat, rep, 'g');
    if(r.indexOf(pat) !== -1) {
 r = src.split(pat).join(rep);
    }
    return r;
}
function h$jsstringReplicateChar(n, ch) {
    ;
    return h$jsstringReplicate(n, h$jsstringSingleton(ch));
}
function h$jsstringIsInteger(str) {
    return /^-?\d+$/.test(str);
}
function h$jsstringIsNatural(str) {
    return /^\d+$/.test(str);
}
function h$jsstringReadInt(str) {
    if(!/^-?\d+/.test(str)) return null;
    var x = parseInt(str, 10);
    var x0 = x|0;
    return (x===x0) ? x0 : null;
}
function h$jsstringLenientReadInt(str) {
    var x = parseInt(str, 10);
    var x0 = x|0;
    return (x===x0) ? x0 : null;
}
function h$jsstringReadWord(str) {
  if(!/^\d+/.test(str)) return null;
  var x = parseInt(str, 10);
  var x0 = x|0;
  if(x0<0) return (x===x0+2147483648) ? x0 : null;
  else return (x===x0) ? x0 : null;
}
function h$jsstringReadDouble(str) {
    return parseFloat(str, 10);
}
function h$jsstringLenientReadDouble(str) {
    return parseFloat(str, 10);
}
function h$jsstringReadInteger(str) {
  ;
  if(!/^(-)?\d+$/.test(str)) {
    return null;
  } else if(str.length <= 9) {
    return (h$c1(h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e, (parseInt(str, 10))));;
  } else {
    return h$ghcjsbn_readInteger(str);
  }
}
function h$jsstringReadInt64(str) {
  if(!/^(-)?\d+$/.test(str)) {
      { h$ret1 = (0); h$ret2 = (0); return (0); };
  }
  if(str.charCodeAt(0) === 45) {
    return h$jsstringReadValue64(str, 1, true);
  } else {
    return h$jsstringReadValue64(str, 0, false);
  }
}
function h$jsstringReadWord64(str) {
  if(!/^\d+$/.test(str)) {
    { h$ret1 = (0); h$ret2 = (0); return (0); };
  }
  return h$jsstringReadValue64(str, 0, false);
}
var h$jsstringLongs = null;
function h$jsstringReadValue64(str, start, negate) {
  var l = str.length, i = start;
  while(i < l) {
    if(str.charCodeAt(i) !== 48) break;
    i++;
  }
  if(i >= l) { h$ret1 = (0); h$ret2 = (0); return (1); };
  if(h$jsstringLongs === null) {
    h$jsstringLongs = [];
    for(var t=10; t<=1000000000; t*=10) {
      h$jsstringLongs.push(goog.math.Long.fromInt(t));
    }
  }
  var li = l-i;
  if(li < 10 && !negate) {
    { h$ret1 = (0); h$ret2 = (parseInt(str.substr(i), 10)); return (1); };
  }
  var r = goog.math.Long.fromInt(parseInt(str.substr(li,9),10));
  li += 9;
  while(li < l) {
    r = r.multiply(h$jsstringLongs[Math.min(l-li-1,8)])
         .add(goog.math.Long.fromInt(parseInt(str.substr(li,9), 10)));
    li += 9;
  }
  if(negate) {
    r = r.negate();
  }
  { h$ret1 = (r.getHighBits()); h$ret2 = (r.getLowBits()); return (1); };
}
function h$jsstringExecRE(i, str, re) {
    re.lastIndex = i;
    var m = re.exec(str);
    if(m === null) return -1;
    var a = [], x, j = 1, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(true) {
 x = m[j];
 if(typeof x === 'undefined') break;
 a[j-1] = x;
 j++;
    }
    j-=1;
    while(--j>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[j])))), (r)));
    { h$ret1 = (m[0]); h$ret2 = (r); return (m.index); };
}
function h$jsstringReplaceRE(pat, replacement, str) {
    return str.replace(pat, replacement);
}
function h$jsstringSplitRE(limit, re, str) {
    re.lastIndex = i;
    var s = (limit < 0) ? str.split(re) : str.split(re, limit);
    var i = s.length, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(--i>=0) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (a[i])))), (r)));
    return r;
}
function h$jsstringRawChunksOf(k, x) {
    var l = x.length;
    if(l === 0) return h$ghczmprimZCGHCziTypesziZMZN;
    if(l <= k) return (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x)))), (h$ghczmprimZCGHCziTypesziZMZN)));
    var r=h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=ls-k;i>=0;i-=k) r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(i,i+k))))), (r)));
    return r;
}
function h$jsstringRawSplitAt(k, x) {
    if(k === 0) return (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,(h$jsstringEmpty),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x))))));
    if(k >= x.length) return (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x)))),(h$jsstringEmpty)));
    return (h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(0,k))))),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (x.substr(k)))))));
}
function h$foreignListProps(o) {
    var r = HS_NIL;
    if(typeof o === 'undefined' || o === null) return null;
    throw "h$foreignListProps";
}
function h$textToString(arr, off, len) {
    var a = [];
    var end = off+len;
    var k = 0;
    var u1 = arr.u1;
    var s = '';
    for(var i=off;i<end;i++) {
 var cc = u1[i];
 a[k++] = cc;
 if(k === 60000) {
     s += String.fromCharCode.apply(this, a);
     k = 0;
     a = [];
 }
    }
    return s + String.fromCharCode.apply(this, a);
}
function h$textFromString(s) {
    var l = s.length;
    var b = h$newByteArray(l * 2);
    var u1 = b.u1;
    for(var i=l-1;i>=0;i--) u1[i] = s.charCodeAt(i);
    { h$ret1 = (l); return (b); };
}
function h$lazyTextToString(txt) {
    var s = '';
    while(((txt).f.a === 2)) {
        var head = ((txt));
        s += h$textToString(((head).d1), ((head).d2.d1), ((head).d2.d2));
        txt = ((txt).d2.d3);
    }
    return s;
}
function h$safeTextFromString(x) {
    if(typeof x !== 'string') {
 { h$ret1 = (0); return (null); };
    }
    return h$textFromString(x);
}
function h$allProps(o) {
    var a = [], i = 0;
    for(var p in o) a[i++] = p;
    return a;
}
function h$listProps(o) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var p in o) { r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (p)))), (r))); }
    return r;
}
function h$listAssocs(o) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var p in o) { r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c2(h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e,((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (p)))),((h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, (o[p]))))))), (r))); }
    return r;
}
function h$isNumber(o) {
    return typeof(o) === 'number';
}
function h$isObject(o) {
    return typeof(o) === 'object';
}
function h$isString(o) {
    return typeof(o) === 'string';
}
function h$isSymbol(o) {
    return typeof(o) === 'symbol';
}
function h$isBoolean(o) {
    return typeof(o) === 'boolean';
}
function h$isFunction(o) {
    return typeof(o) === 'function';
}
function h$jsTypeOf(o) {
    var t = typeof(o);
    if(t === 'undefined') return 0;
    if(t === 'object') return 1;
    if(t === 'boolean') return 2;
    if(t === 'number') return 3;
    if(t === 'string') return 4;
    if(t === 'symbol') return 5;
    if(t === 'function') return 6;
    return 7;
}
function h$jsonTypeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            return 5;
        } else if (!o) {
            return 0;
        } else {
            return 6;
        }
    }
}
function h$sendXHR(xhr, d, cont) {
    xhr.addEventListener('error', function () {
 cont(2);
    });
    xhr.addEventListener('abort', function() {
 cont(1);
    });
    xhr.addEventListener('load', function() {
 cont(0);
    });
    if(d) {
 xhr.send(d);
    } else {
 xhr.send();
    }
}
function h$createWebSocket(url, protocols) {
  return new WebSocket(url, protocols);
}
function h$openWebSocket(ws, mcb, ccb, c) {
  if(ws.readyState !== 0) {
    throw new Error("h$openWebSocket: unexpected readyState, socket must be CONNECTING");
  }
  ws.lastError = null;
  ws.onopen = function() {
    if(mcb) {
      ws.onmessage = mcb;
    }
    if(ccb || mcb) {
      ws.onclose = function(ce) {
        if(ws.onmessage) {
          h$release(ws.onmessage);
          ws.onmessage = null;
        }
        if(ccb) {
          h$release(ccb);
          ccb(ce);
        }
      };
    };
    ws.onerror = function(err) {
      ws.lastError = err;
      if(ws.onmessage) {
        h$release(ws.onmessage);
        ws.onmessage = null;
      }
      ws.close();
    };
    c(null);
  };
  ws.onerror = function(err) {
    if(ccb) h$release(ccb);
    if(mcb) h$release(mcb);
    ws.onmessage = null;
    ws.close();
    c(err);
  };
}
function h$closeWebSocket(status, reason, ws) {
  ws.onerror = null;
  if(ws.onmessage) {
    h$release(ws.onmessage);
    ws.onmessage = null;
  }
  ws.close(status, reason);
}
goog.provide('goog.crypt.Hash');
goog.crypt.Hash = function() {
  this.blockSize = -1;
};
goog.crypt.Hash.prototype.reset = goog.abstractMethod;
goog.crypt.Hash.prototype.update = goog.abstractMethod;
goog.crypt.Hash.prototype.digest = goog.abstractMethod;
goog.provide('goog.crypt.Md5');
goog.require('goog.crypt.Hash');
goog.crypt.Md5 = function() {
  goog.crypt.Md5.base(this, 'constructor');
  this.blockSize = 512 / 8;
  this.chain_ = new Array(4);
  this.block_ = new Array(this.blockSize);
  this.blockLength_ = 0;
  this.totalLength_ = 0;
  this.reset();
};
goog.inherits(goog.crypt.Md5, goog.crypt.Hash);
goog.crypt.Md5.prototype.reset = function() {
  this.chain_[0] = 0x67452301;
  this.chain_[1] = 0xefcdab89;
  this.chain_[2] = 0x98badcfe;
  this.chain_[3] = 0x10325476;
  this.blockLength_ = 0;
  this.totalLength_ = 0;
};
goog.crypt.Md5.prototype.compress_ = function(buf, opt_offset) {
  if (!opt_offset) {
    opt_offset = 0;
  }
  var X = new Array(16);
  if (goog.isString(buf)) {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf.charCodeAt(opt_offset++)) |
             (buf.charCodeAt(opt_offset++) << 8) |
             (buf.charCodeAt(opt_offset++) << 16) |
             (buf.charCodeAt(opt_offset++) << 24);
    }
  } else {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf[opt_offset++]) |
             (buf[opt_offset++] << 8) |
             (buf[opt_offset++] << 16) |
             (buf[opt_offset++] << 24);
    }
  }
  var A = this.chain_[0];
  var B = this.chain_[1];
  var C = this.chain_[2];
  var D = this.chain_[3];
  var sum = 0;
  sum = (A + (D ^ (B & (C ^ D))) + X[0] + 0xd76aa478) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[1] + 0xe8c7b756) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[2] + 0x242070db) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[3] + 0xc1bdceee) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[4] + 0xf57c0faf) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[5] + 0x4787c62a) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[6] + 0xa8304613) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[7] + 0xfd469501) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[8] + 0x698098d8) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[9] + 0x8b44f7af) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[10] + 0xffff5bb1) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[11] + 0x895cd7be) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[12] + 0x6b901122) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[13] + 0xfd987193) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[14] + 0xa679438e) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[15] + 0x49b40821) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (C ^ (D & (B ^ C))) + X[1] + 0xf61e2562) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[6] + 0xc040b340) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[11] + 0x265e5a51) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[0] + 0xe9b6c7aa) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[5] + 0xd62f105d) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[10] + 0x02441453) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[15] + 0xd8a1e681) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[4] + 0xe7d3fbc8) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[9] + 0x21e1cde6) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[14] + 0xc33707d6) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[3] + 0xf4d50d87) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[8] + 0x455a14ed) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[13] + 0xa9e3e905) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[2] + 0xfcefa3f8) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[7] + 0x676f02d9) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[12] + 0x8d2a4c8a) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (B ^ C ^ D) + X[5] + 0xfffa3942) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[8] + 0x8771f681) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[11] + 0x6d9d6122) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[14] + 0xfde5380c) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[1] + 0xa4beea44) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[4] + 0x4bdecfa9) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[7] + 0xf6bb4b60) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[10] + 0xbebfbc70) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[13] + 0x289b7ec6) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[0] + 0xeaa127fa) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[3] + 0xd4ef3085) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[6] + 0x04881d05) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[9] + 0xd9d4d039) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[12] + 0xe6db99e5) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[15] + 0x1fa27cf8) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[2] + 0xc4ac5665) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (C ^ (B | (~D))) + X[0] + 0xf4292244) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[7] + 0x432aff97) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[14] + 0xab9423a7) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[5] + 0xfc93a039) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[12] + 0x655b59c3) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[3] + 0x8f0ccc92) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[10] + 0xffeff47d) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[1] + 0x85845dd1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[8] + 0x6fa87e4f) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[15] + 0xfe2ce6e0) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[6] + 0xa3014314) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[13] + 0x4e0811a1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[4] + 0xf7537e82) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[11] + 0xbd3af235) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[2] + 0x2ad7d2bb) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[9] + 0xeb86d391) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  this.chain_[0] = (this.chain_[0] + A) & 0xffffffff;
  this.chain_[1] = (this.chain_[1] + B) & 0xffffffff;
  this.chain_[2] = (this.chain_[2] + C) & 0xffffffff;
  this.chain_[3] = (this.chain_[3] + D) & 0xffffffff;
};
goog.crypt.Md5.prototype.update = function(bytes, opt_length) {
  if (!goog.isDef(opt_length)) {
    opt_length = bytes.length;
  }
  var lengthMinusBlock = opt_length - this.blockSize;
  var block = this.block_;
  var blockLength = this.blockLength_;
  var i = 0;
  while (i < opt_length) {
    if (blockLength == 0) {
      while (i <= lengthMinusBlock) {
        this.compress_(bytes, i);
        i += this.blockSize;
      }
    }
    if (goog.isString(bytes)) {
      while (i < opt_length) {
        block[blockLength++] = bytes.charCodeAt(i++);
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          break;
        }
      }
    } else {
      while (i < opt_length) {
        block[blockLength++] = bytes[i++];
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          break;
        }
      }
    }
  }
  this.blockLength_ = blockLength;
  this.totalLength_ += opt_length;
};
goog.crypt.Md5.prototype.digest = function() {
  var pad = new Array((this.blockLength_ < 56 ?
                       this.blockSize :
                       this.blockSize * 2) - this.blockLength_);
  pad[0] = 0x80;
  for (var i = 1; i < pad.length - 8; ++i) {
    pad[i] = 0;
  }
  var totalBits = this.totalLength_ * 8;
  for (var i = pad.length - 8; i < pad.length; ++i) {
    pad[i] = totalBits & 0xff;
    totalBits /= 0x100;
  }
  this.update(pad);
  var digest = new Array(16);
  var n = 0;
  for (var i = 0; i < 4; ++i) {
    for (var j = 0; j < 32; j += 8) {
      digest[n++] = (this.chain_[i] >>> j) & 0xff;
    }
  }
  return digest;
};
function h$base_access(file, file_off, mode, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_chmod(file, file_off, mode, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_close(fd, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo) {
        delete h$base_fds[fd];
        if(--fdo.refs < 1) {
          ;
          if(fdo.close) {
            fdo.close(fd, fdo, c);
          } else {
            c(0);
          }
        } else {
          ;
          c(0);
        }
    } else {
        ;
        h$errno = 22;
        c(-1);
    }
}
function h$base_dup(fd, c) {
    h$base_dup2(fd, h$base_fdN--, c);
}
function h$base_dup2(fd, new_fd, c) {
   ;
    var fdo = h$base_fds[fd];
    if(!fdo) {
      ;
      h$errno = 22;
      c(-1);
    } else {
      var new_fdo = h$base_fds[new_fd];
      function f() {
        h$base_fds[new_fd] = fdo;
        fdo.refs++;
        c(new_fd);
      }
      if(new_fdo) {
        ;
        h$base_close(new_fd, f);
      } else {
        f();
      }
    }
}
function h$base_fstat(fd, stat, stat_off, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_isatty(fd) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && typeof fdo.isatty !== 'undefined') {
      if(typeof fdo.isatty === 'function') return fdo.isatty() ? 1 : 0;
      return fdo.isatty ? 1 : 0;
    }
    return 0;
}
function h$base_lseek(fd, pos_1, pos_2, whence, c) {
    ;
        h$unsupported();
        c(-1, -1);
}
function h$base_lstat(file, file_off, stat, stat_off, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_open(file, file_off, how, mode, c) {
        h$unsupported(-1, c);
}
function h$base_read(fd, buf, buf_off, n, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.read) {
        fdo.read(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.read(fd, buf.u8, buf_off, n, null, function(err, bytesRead, buf0) {
            h$handleErrnoC(err, -1, bytesRead, c);
        });
    }
}
function h$base_stat(file, file_off, stat, stat_off, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_umask(mode) {
    ;
    return 0;
}
function h$base_write(fd, buf, buf_off, n, c) {
    ;
    var fdo = h$base_fds[fd];
    if(fdo && fdo.write) {
        fdo.write(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.write(fd, buf.u8, buf_off, n, function(err, bytesWritten, buf0) {
            h$handleErrnoC(err, -1, bytesWritten, c);
        });
    }
}
function h$base_ftruncate(fd, pos_1, pos_2, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_unlink(file, file_off, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_getpid() {
    ;
    return 0;
}
function h$base_link(file1, file1_off, file2, file2_off, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_mkfifo(file, file_off, mode, c) {
    throw "h$base_mkfifo";
}
function h$base_sigemptyset(sigset, sigset_off) {
    return 0;
}
function h$base_sigaddset(sigset, sigset_off, sig) {
    return 0;
}
function h$base_sigprocmask(sig, sigset1, sigset1_off, sigset2, sigset2_off) {
    return 0;
}
function h$base_tcgetattr(attr, termios, termios_off) {
    return 0;
}
function h$base_tcsetattr(attr, val, termios, termios_off) {
    return 0;
}
function h$base_utime(file, file_off, timbuf, timbuf_off, c) {
    ;
        h$unsupported(-1, c);
}
function h$base_waitpid(pid, stat, stat_off, options, c) {
    throw "h$base_waitpid";
}
              var h$base_o_rdonly = 0x00000;
              var h$base_o_wronly = 0x00001;
              var h$base_o_rdwr = 0x00002;
              var h$base_o_accmode = 0x00003;
              var h$base_o_append = 0x00008;
              var h$base_o_creat = 0x00200;
              var h$base_o_trunc = 0x00400;
              var h$base_o_excl = 0x00800;
              var h$base_o_noctty = 0x20000;
              var h$base_o_nonblock = 0x00004;
              var h$base_o_binary = 0x00000;
function h$base_c_s_isreg(mode) {
    return 1;
}
function h$base_c_s_ischr(mode) {
    return 0;
}
function h$base_c_s_isblk(mode) {
    return 0;
}
function h$base_c_s_isdir(mode) {
    return 0;
}
function h$base_c_s_isfifo(mode) {
    return 0;
}
              var h$base_sizeof_stat = 40;
function h$base_st_mtime(stat, stat_off) {
    { h$ret1 = (stat.i3[(stat_off>>2)+4]); return (stat.i3[(stat_off>>2)+3]); };
}
function h$base_st_size(stat, stat_off) {
    { h$ret1 = (stat.i3[(stat_off>>2)+2]); return (stat.i3[(stat_off>>2)+1]); };
}
function h$base_st_mode(stat, stat_off) {
    return stat.i3[stat_off>>2];
}
function h$base_st_dev(stat, stat_off) {
    return stat.i3[(stat_off>>2)+5];
}
function h$base_st_ino(stat, stat_off) {
    { h$ret1 = (stat.i3[(stat_off>>2)+7]); return (stat.i3[(stat_off>>2)+6]); };
}
              var h$base_echo = 1;
              var h$base_tcsanow = 2;
              var h$base_icanon = 4;
              var h$base_vmin = 8;
              var h$base_vtime = 16;
              var h$base_sigttou = 0;
              var h$base_sig_block = 0;
              var h$base_sig_setmask = 0;
              var h$base_f_getfl = 0;
              var h$base_f_setfl = 0;
              var h$base_f_setfd = 0;
              var h$base_fd_cloexec = 0;
              var h$base_sizeof_termios = 4;
              var h$base_sizeof_sigset_t = 4;
function h$base_lflag(termios, termios_off) {
    return 0;
}
function h$base_poke_lflag(termios, termios_off, flag) {
    return 0;
}
function h$base_ptr_c_cc(termios, termios_off) {
    { h$ret1 = (0); return (h$newByteArray(8)); };
}
              var h$base_default_buffer_size = 32768;
function h$base_c_s_issock(mode) {
    return 0;
}
              var h$base_SEEK_SET = 0;
              var h$base_SEEK_CUR = 1;
              var h$base_SEEK_END = 2;
function h$base_set_saved_termios(a, b, c) {
    { h$ret1 = (0); return (null); };
}
function h$base_get_saved_termios(r) {
    { h$ret1 = (0); return (null); };
}
function h$lockFile(fd, dev, ino, for_writing) {
    ;
    return 0;
}
function h$unlockFile(fd) {
    ;
    return 0;
}
var h$base_readStdin , h$base_writeStderr, h$base_writeStdout;
var h$base_isattyStdin = false, h$base_isattyStdout = false, h$base_isattyStderr = false;
var h$base_closeStdin = null, h$base_closeStderr = null, h$base_closeStdout = null;
var h$base_readFile, h$base_writeFile, h$base_closeFile;
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
var h$base_stdin_fd =
  { read: h$base_readStdin
  , close: h$base_closeStdin
  , isatty: h$base_isattyStdin
  , refs: 1
  };
var h$base_stdout_fd =
  { write: h$base_writeStdout
  , close: h$base_closeStdout
  , isatty: h$base_isattyStdout
  , refs: 1
  };
var h$base_stderr_fd =
  { write: h$base_writeStderr
  , close: h$base_closeStderr
  , isatty: h$base_isattyStderr
  , refs: 1
  };
var h$base_fdN = -2;
var h$base_fds = [h$base_stdin_fd, h$base_stdout_fd, h$base_stderr_fd];
function h$shutdownHaskellAndExit(code, fast) {
    h$exitProcess(code);
}
function h$rand() {
  return (32768 * Math.random()) & 32767;
}
function h$stg_sig_install(sigNo, actionCode, sigSet_d, sigSet_o) {
  return 0;
}
function h$get_current_timezone_seconds(t, pdst_v, pdst_o, pname_v, pname_o) {
    var d = new Date(t * 1000);
    var now = new Date();
    var jan = new Date(now.getFullYear(),0,1);
    var jul = new Date(now.getFullYear(),6,1);
    var stdOff = Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
    var isDst = d.getTimezoneOffset() < stdOff;
    var tzo = d.getTimezoneOffset();
    pdst_v.dv.setInt32(pdst_o, isDst ? 1 : 0, true);
    if(!pname_v.arr) pname_v.arr = [];
    var offstr = tzo < 0 ? ('+' + (tzo/-60)) : ('' + (tzo/-60));
    pname_v.arr[pname_o] = [h$encodeUtf8("UTC" + offstr), 0];
    return (-60*tzo)|0;
}
function h$clock_gettime(when, p_d, p_o) {
  var is64 = p_d.i3.length == 4 && p_o == 0;
  var o = p_o >> 2,
      t = Date.now ? Date.now() : new Date().getTime(),
      tf = Math.floor(t / 1000),
      tn = 1000000 * (t - (1000 * tf));
  if(is64) {
    p_d.i3[o] = tf|0;
    p_d.i3[o+1] = 0;
    p_d.i3[o+2] = tn|0;
    p_d.i3[o+3] = 0;
  } else {
    p_d.i3[o] = tf|0;
    p_d.i3[o+1] = tn|0;
  }
  return 0;
}
function h$clock_getres(when, p_d, p_o) {
  return 0;
}
function h$_hs_text_memcpy(dst_v,dst_v_zero,dst_o2,src_v,src_o_zero,src_o2,n) {
  return h$memcpy(dst_v,2*dst_o2,src_v,2*src_o2,2*n);
}
function h$_hs_text_memcmp(a_v,a_o_zero,a_o2,b_v,b_o_zero,b_o2,n) {
  return h$memcmp(a_v,2*a_o2,b_v,2*b_o2,2*n);
}
var h$_text_utf8d =
   [
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12];
function h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero
                                         , destoff_v, destoff_o
                                         , src_v, src_o
                                         , src_end_v, src_end_o
                                         , s
                                         ) {
  if(src_v === null || src_end_v === null) {
    { h$ret1 = (src_end_o); return (null); };
  }
  var dsto = destoff_v.dv.getUint32(destoff_o,true) << 1;
  var srco = src_o;
  var state = s.state;
  var codepoint = s.codepoint;
  var ddv = dest_v.dv;
  var sdv = src_v.dv;
  function decode(b) {
    var type = h$_text_utf8d[b];
    codepoint = (state !== 0) ?
      (b & 0x3f) | (codepoint << 6) :
      (0xff >>> type) & b;
    state = h$_text_utf8d[256 + state + type];
    return state;
  }
  while (srco < src_end_o) {
    if(decode(sdv.getUint8(srco++)) !== 0) {
      if(state !== 12) {
        continue;
      } else {
        break;
      }
    }
    if (codepoint <= 0xffff) {
      ddv.setUint16(dsto,codepoint,true);
      dsto += 2;
    } else {
      ddv.setUint16(dsto,(0xD7C0 + (codepoint >>> 10)),true);
      ddv.setUint16(dsto+2,(0xDC00 + (codepoint & 0x3FF)),true);
      dsto += 4;
    }
    s.last = srco;
  }
  s.state = state;
  s.codepoint = codepoint;
  destoff_v.dv.setUint32(destoff_o,dsto>>1,true);
  { h$ret1 = (s.last); return (src_v); };
}
function h$_hs_text_decode_utf8_state( dest_v, dest_o_zero
                                     , destoff_v, destoff_o
                                     , src_v, src_o
                                     , srcend_v, srcend_o
                                     , codepoint0_v, codepoint0_o
                                     , state0_v, state0_o
                                     ) {
  var s = { state: state0_v.dv.getUint32(state0_o, true)
          , codepoint: codepoint0_v.dv.getUint32(codepoint0_o, true)
          , last: src_o
          };
  var ret, ret1;
  { (ret) = (h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero , destoff_v, destoff_o , src_v.arr[src_o][0], src_v.arr[src_o][1] , srcend_v, srcend_o , s )); (ret1) = h$ret1; };
  src_v.arr[src_o][1] = s.last;
  state0_v.dv.setUint32(state0_o, s.state, true);
  codepoint0_v.dv.setUint32(codepoint0_o, s.codepoint, true);
  { h$ret1 = (ret1); return (ret); };
}
function h$_hs_text_decode_utf8( dest_v, dest_o_zero
                               , destoff_v, destoff_o
                               , src_v, src_o
                               , srcend_v, srcend_o
                               ) {
  var s = { state: 0
          , codepoint: 0
          , last: src_o
          };
  var ret, ret1;
  { (ret) = (h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero , destoff_v, destoff_o , src_v, src_o , srcend_v, srcend_o , s )); (ret1) = h$ret1; };
  { h$ret1 = (ret1); return (ret); };
}
function h$_hs_text_decode_latin1(dest_d, dest_o_zero, src_d, src_o, srcend_d, srcend_o) {
  var p = src_o;
  var d = 0;
  var su8 = src_d.u8;
  var su3 = src_d.u3;
  var du1 = dest_d.u1;
  while(p != srcend_o && p & 3) {
    du1[d++] = su8[p++];
  }
  if(su3) {
    while (p < srcend_o - 3) {
      var w = su3[p>>2];
      du1[d++] = w & 0xff;
      du1[d++] = (w >>> 8) & 0xff;
      du1[d++] = (w >>> 16) & 0xff;
      du1[d++] = (w >>> 32) & 0xff;
      p += 4;
    }
  }
  while (p != srcend_o)
    du1[d++] = su8[p++];
}
function h$_hs_text_encode_utf8(destp_v, destp_o, src_v, src_o_zero, srcoff, srclen) {
  var dest_v = destp_v.arr[destp_o][0];
  var dest_o = destp_v.arr[destp_o][1];
  var src = srcoff;
  var dest = dest_o;
  var srcend = src + srclen;
  var srcu1 = src_v.u1;
  if(!srcu1) throw "h$_hs_text_encode_utf8: invalid alignment for source";
  var srcu3 = src_v.u3;
  var destu8 = dest_v.u8;
  while(src < srcend) {
    while(srcu3 && !(src & 1) && srcend - src >= 2) {
      var w = srcu3[src>>1];
      if(w & 0xFF80FF80) break;
      destu8[dest++] = w & 0xFFFF;
      destu8[dest++] = w >>> 16;
      src += 2;
    }
    while(src < srcend) {
      var w = srcu1[src++];
      if(w <= 0x7F) {
        destu8[dest++] = w;
        break;
      } else if(w <= 0x7FF) {
        destu8[dest++] = (w >> 6) | 0xC0;
        destu8[dest++] = (w & 0x3f) | 0x80;
      } else if(w < 0xD800 || w > 0xDBFF) {
        destu8[dest++] = (w >>> 12) | 0xE0;
        destu8[dest++] = ((w >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (w & 0x3F) | 0x80;
      } else {
        var c = ((w - 0xD800) << 10) + (srcu1[src++] - 0xDC00) + 0x10000;
        destu8[dest++] = (c >>> 18) | 0xF0;
        destu8[dest++] = ((c >> 12) & 0x3F) | 0x80;
        destu8[dest++] = ((c >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (c & 0x3F) | 0x80;
      }
    }
  }
  destp_v.arr[destp_o][1] = dest;
}
function h$hsprimitive_memcpy(dst_d, dst_o, doff, src_d, src_o, soff, len) {
  return h$primitive_memmove(dst_d, dst_o, doff, src_d, src_o, len);
}
function h$hsprimitive_memmove(dst_d, dst_o, doff, src_d, src_o, soff, len) {
  if(len === 0) return;
  var du8 = dst_d.u8, su8 = src_d.u8;
  for(var i=len-1;i>=0;i--) {
    du8[dst_o+i] = su8[src_o+i];
  }
}
function h$hsprimitive_memsetba_Word8 (p_d, off, n, x) { if(n > 0) { if(p_d.u8.fill) p_d.u8.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.u8[i] = x; } }
function h$hsprimitive_memsetba_Word16 (p_d, off, n, x) { if(n > 0) { if(p_d.u1.fill) p_d.u1.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.u1[i] = x; } }
function h$hsprimitive_memsetba_Word32 (p_d, off, n, x) { if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memsetba_Word (p_d, off, n, x) { if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memsetba_Float (p_d, off, n, x) { if(n > 0) { if(p_d.f3.fill) p_d.f3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.f3[i] = x; } }
function h$hsprimitive_memsetba_Double (p_d, off, n, x) { if(n > 0) { if(p_d.f6.fill) p_d.f6.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.f6[i] = x; } }
function h$hsprimitive_memsetba_Char (p_d, off, n, x) { if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, off, off + n); else for(var i=off; i<off+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memset_Word8 (p_d, p_o, off, n, x) { var start = (p_o >> 0) + off; if(n > 0) { if(p_d.u8.fill) p_d.u8.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.u8[i] = x; } }
function h$hsprimitive_memset_Word16 (p_d, p_o, off, n, x) { var start = (p_o >> 1) + off; if(n > 0) { if(p_d.u1.fill) p_d.u1.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.u1[i] = x; } }
function h$hsprimitive_memset_Word32 (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memset_Word (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memset_Float (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.f3.fill) p_d.f3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.f3[i] = x; } }
function h$hsprimitive_memset_Double (p_d, p_o, off, n, x) { var start = (p_o >> 3) + off; if(n > 0) { if(p_d.f6.fill) p_d.f6.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.f6[i] = x; } }
function h$hsprimitive_memset_Char (p_d, p_o, off, n, x) { var start = (p_o >> 2) + off; if(n > 0) { if(p_d.i3.fill) p_d.i3.fill(x, start, start + n); else for(var i=start; i<start+n; i++) p_d.i3[i] = x; } }
function h$hsprimitive_memsetba_Word64(p_d, off, n, x_1, x_2) {
  h$hsprimitive_memset_Word64(p_d, 0, off, n, x_1, x_2);
}
function h$hsprimitive_memset_Word64(p_d, p_o, off, n, x_1, x_2) {
  var start = (p_o >> 3) + off;
  if(n > 0) {
    var pi3 = p_d.i3;
    for(var i = 0; i < n; i++) {
      var o = (start + i) << 1;
      pi3[o] = x_1;
      pi3[o+1] = x_2;
    }
  }
}
function h$hsprimitive_memset_Ptr(p_d, p_o, off, n, x_1, x_2) {
  if(n > 0) {
    if(!p_d.arr) p_d.arr = [];
    var a = p_d.arr;
    for(var i = 0; i < n; i++) {
      a[p_o + ((off + i) << 2)] = [x_1, x_2];
    }
  }
}
function h$hashable_fnv_hash_offset(str_a, str_o_zero, o, len, hash) {
  return h$hashable_fnv_hash(str_a, o, len, hash);
}
function h$hashable_fnv_hash(str_d, str_o, len, hash) {
  if(len > 0) {
    var d = str_d.u8;
    for(var i=0;i<len;i++) {
      hash = h$mulInt32(hash, 16777619) ^ d[str_o+i];
    }
  }
  return hash;
}
function h$hashable_getRandomBytes(dest_d, dest_o, len) {
  if(len > 0) {
    var d = dest_d.u8;
    for(var i=0;i<len;i++) {
      d[dest_o+i] = Math.floor(Math.random() * 256);
    }
  }
  return len;
}
function h$ghcjs_currentWindow() {
  return window;
};
function h$ghcjs_currentDocument() {
  return document;
};
function h$isFloat (n) {
  return n===+n && n!==(n|0);
}
function h$isInteger (n) {
  return n===+n && n===(n|0);
}
function h$typeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            return 5;
        } else if (!o) {
            return 0;
        } else {
            return 6;
        }
    }
}
function h$flattenObj(o) {
    var l = [], i = 0;
    for (var prop in o) {
        l[i++] = [prop, o[prop]];
    }
    return l;
}
function h$buildObject() {
    var r = {}, l = arguments.length;
    for(var i = 0; i < l; i += 2) {
        var k = arguments[i], v = arguments[i+1];
        r[k] = v;
    }
    return r;
}
function h$buildObjectFromList(xs) {
    var r = {}, k, v, t;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
        xs = ((xs).d2);
        t = ((xs).d2);
        if(((t).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
            k = ((xs).d1);
            v = ((t).d1);
            xs = ((t).d2);
            r[k] = v;
        } else {
            return r;
        }
    }
    return r;
}
function h$buildObjectFromTupList(xs) {
    var r = {};
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 var h = ((xs).d1);
 xs = ((xs).d2);
 r[((((h).d1)).d1)] = ((((h).d2)).d1);
    }
    return r;
}
function h$fps_reverse(a_v, a_o, b_v, b_o, n) {
    if(n > 0) {
        var au8 = a_v.u8, bu8 = b_v.u8;
        for(var i=0;i<n;i++) {
            au8[a_o+n-i-1] = bu8[b_o+i];
        }
    }
}
function h$fps_intersperse(a_v,a_o,b_v,b_o,n,c) {
    if(n > 0) {
        var au8 = a_v.u8, bu8 = b_v.u8, dst_o = a_o;
        for(var i=0;i<n-1;i++) {
            au8[dst_o] = bu8[b_o+i];
            au8[dst_o+1] = c;
            dst_o += 2;
        }
        au8[dst_o] = bu8[b_o+n-1];
    }
}
function h$fps_maximum(a_v,a_o,n) {
    if(n > 0) {
        var au8 = a_v.u8, max = au8[a_o];
        for(var i=1;i<n;i++) {
            var c = au8[a_o+i];
            if(c > max) { max = c; }
        }
        return max;
    }
    return 0;
}
function h$fps_minimum(a_v,a_o,n) {
    if(n > 0) {
        var au8 = a_v.u8, min = a_v.u8[a_o];
        for(var i=1;i<n;i++) {
            var c = au8[a_o+i];
            if(c < min) { min = c; }
        }
        return min;
    }
    return 255;
}
function h$fps_count(a_v,a_o,n,c) {
    if(n > 0) {
        var au8 = a_v.u8, count = 0;
        for(var i=0;i<n;i++) {
            if(au8[a_o+i] === c) { count++; }
        }
        return count|0;
    }
    return 0;
}
function h$fps_memcpy_offsets(dst_d, dst_o, dst_off
                              , src_d, src_o, src_off, n) {
    return memcpy(dst_d, dst_o + dst_off, src_d, src_o + src_off, n);
}
var h$_hs_bytestring_digits = [48,49,50,51,52,53,54,55,56,57,97,98,99,100,101,102];
var h$_hs_bytestring_l10 = goog.math.Long.fromBits(10, 0);
function h$_hs_bytestring_int_dec(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free, x_tmp;
    var bu8 = buf_d.u8;
    if(x < 0) {
        bu8[ptr++] = 45;
        buf_o++;
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x * 10 - x_tmp];
        if(x === 0) {
            { h$ret1 = (ptr); return (buf_d); };
        } else {
            x = -x;
        }
    }
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while (x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_long_long_int_dec(x_a, x_b, buf_d, buf_o) {
    var l10 = h$_hs_bytestring_l10;
    var x = goog.math.Long.fromBits(x_b, x_a);
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    if(x.isNegative()) {
        bu8[ptr++] = 45;
        buf_o++;
        x_tmp = x;
        x = x.div(l10);
        bu8[ptr++] = h$_hs_bytestring_digits[x.multiply(l10).subtract(x_tmp).getLowBits()];
        if(x.isZero()) {
            { h$ret1 = (ptr); return (buf_d); };
        } else {
            x = x.negate();
        }
    }
    do {
        x_tmp = x;
        x = x.div(l10);
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(l10))];
    } while (!x.isZero());
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_uint_dec(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    var x_tmp;
    if(x < 0) x += 4294967296;
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[ptr++] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while(x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_long_long_uint_dec(x_a, x_b, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    var x = h$ghcjsbn_mkBigNat_ww(x_a, x_b), q = [], r;
    do {
        r = h$ghcjsbn_quotRem_bw(q, x, 10);
        x = q;
        q = [];
        bu8[ptr++] = h$_hs_bytestring_digits[r];
    } while(!h$ghcjsbn_isZero_b(x));
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_int_dec_padded9(x, buf_d, buf_o) {
    var max_width_int32_dec = 9;
    var ptr = buf_o + max_width_int32_dec;
    var bu8 = buf_d.u8;
    var x_tmp;
    do {
        x_tmp = x;
        x = (x / 10) | 0;
        bu8[--ptr] = h$_hs_bytestring_digits[x_tmp - x * 10];
    } while(x);
    while (buf_o < ptr) { bu8[--ptr] = 48; }
}
function h$_hs_bytestring_long_long_int_dec_padded18(x_a, x_b, buf_d, buf_o) {
    var l10 = h$_hs_bytestring_l10;
    var max_width_int64_dec = 18;
    var ptr = buf_o + max_width_int64_dec;
    var bu8 = buf_d.u8;
    var x = goog.math.Long.fromBits(x_b, x_a);
    do {
        x_tmp = x;
        x = x.div(l10);
        bu8[--ptr] = h$_hs_bytestring_digits[x_tmp.subtract(x.multiply(l10))];
    } while (!x.isZero());
    while (buf_o < ptr) { bu8[--ptr] = 48; }
}
function h$_hs_bytestring_uint_hex(x, buf_d, buf_o) {
    var c, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    do {
        bu8[ptr++] = h$_hs_bytestring_digits[x & 0xf];
        x >>>= 4;
    } while(x);
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
function h$_hs_bytestring_long_long_uint_hex(x_a, x_b, buf_d, buf_o) {
    var c, i, ptr = buf_o, next_free;
    var bu8 = buf_d.u8;
    if(x_a === 0 && x_b === 0) {
        bu8[ptr++] = 48;
    } else if(x_a === 0) {
      while(x_b !== 0) {
          bu8[ptr++] = h$_hs_bytestring_digits[x_b & 0xf];
          x_b >>>= 4;
      }
    } else {
        for(i=0;i<8;i++) {
            bu8[ptr++] = h$_hs_bytestring_digits[x_b & 0xf];
            x_b >>>= 4;
        }
        while(x_a !== 0) {
            bu8[ptr++] = h$_hs_bytestring_digits[x_a & 0xf];
            x_a >>>= 4;
        }
    }
    next_free = ptr--;
    while(buf_o < ptr) {
        c = bu8[ptr];
        bu8[ptr--] = bu8[buf_o];
        bu8[buf_o++] = c;
    }
    { h$ret1 = (next_free); return (buf_d); };
}
