
def lfsr(i)
  x = 0x80;
  i.times {
    v = x & 1;
    x = x >> 1;
    if v == 1
      x = x ^ 0x8e;
    end
  }
  return x >> 7;
end


def rc(r)
  a = 0;
  7.times { |j|
    t = lfsr(j + 7 * r);
    a |= t << ((1<<j) - 1)
  }
  return a
end

24.times { |r|
  printf("16#%016x#, -- round %d\n", rc(r), r + 1);  
}
