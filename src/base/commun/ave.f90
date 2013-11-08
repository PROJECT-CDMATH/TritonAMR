      function ave(a,b)
      IMPLICIT NONE
      DOUBLE PRECISION a,b,ave,un
      un=1.0
      ave=dsign(un,a)*max(0.0,min(abs(a),dsign(un,a)*b))
      end function ave

