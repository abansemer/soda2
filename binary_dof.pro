FUNCTION binary_dof, bitimage
   ;FUNCTION to roughly emulate a greyscale image from a binary image and decide whether it is in the depth of field.
   IF max(bitimage) gt 1 THEN stop, 'Bitimage must be binary'

   ;Convolution kernel, 3x3 for now since mainly concerned with <100um particles
   k = [[0.7,1,0.7],[1,0,1],[0.7,1,0.7]]
   m = total(convol(float(bitimage), k, edge_constant=0) * bitimage)  ;Rough approximation of greyscale image

   ;Using hand-drawn pixel arrangements to decide whether or not to accept
   area = total(bitimage)
   dof = 0
   CASE area OF
      1: dof = 1
      2: IF m ge 2 THEN dof = 1
      3: IF m ge 5 THEN dof = 1
      4: IF m ge 8 THEN dof = 1
      5: IF m ge 11 THEN dof = 1
      ELSE: IF m ge (area*3) THEN dof = 1
   ENDCASE
   return, dof
END
