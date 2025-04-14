-----------------------------------------------------------------------------
--                               MarTRix                                   -- 
-----------------------------------------------------------------------------
--                                                                         --
--  MarTRix is a tetris-like game for MaRTE OS.                            --
--  This code is distributed for educational purposes only.                --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  email:    alvaro.garcia.cuesta@gmail.com                               --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: blocks.ads                                                       --
--                                                                         --
--  this file contains data structures representing the map and all the    --
--  blocks or pieces (O,L,T,Z...) and their corresponding variations       --
-----------------------------------------------------------------------------
--                        last update: 16 Feb 09                           --
-----------------------------------------------------------------------------
package blocks is
        
        type map_array is array (1..10, 1..20) of Integer;
        
        --  the active are, or map, is represented by an array of integers
        --  where 0 represents an empty block and 1-7 represent an occupied
        --  square and its color
        --  the area is divided in 200 squares (10 x 20)
        
        map1:map_array:=(
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        );
        --  a block is an array of 4 x 4 subblocks ; 
        --  4 orientations x 7 type of blocks = 28 possible block shapes
        type block_array is array (1..4, 1..4) of Integer;
        type block_pointer is Access all block_array;
        type blocks_array is array (1..28) of block_pointer;
        
        
        --  x and y are inversed to make it more visual
        --  maybe should change that
        block_O_1:block_array:=(
                (0,0,0,0),
                (0,1,1,0),
                (0,1,1,0),
                (0,0,0,0)
        );--  block_O_1
        
        block_O_1_pointer:aliased block_array:=block_O_1;
        --  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        
        block_I_1:block_array:=(
                (0,0,0,0),
                (1,1,1,1),
                (0,0,0,0),
                (0,0,0,0)
        );--  block_I_1
        
        block_I_1_pointer:aliased block_array:=block_I_1;
        
        block_I_2:block_array:=(
                (0,0,1,0),
                (0,0,1,0),
                (0,0,1,0),
                (0,0,1,0)
        );--  block_I_2
        
        block_I_2_pointer:aliased block_array:=block_I_2;
        
        --  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        block_S_1:block_array:=(
                (0,0,0,0),
                (0,0,1,1),
                (0,1,1,0),
                (0,0,0,0)
        );--  block_S_1
        
        block_S_1_pointer:aliased block_array:=block_S_1;
        
        block_S_2:block_array:=(
                (0,0,1,0),
                (0,0,1,1),
                (0,0,0,1),
                (0,0,0,0)
        );--  block_S_2
        
        block_S_2_pointer:aliased block_array:=block_S_2;
        --  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        block_Z_1:block_array:=(
                (0,0,0,0),
                (0,1,1,0),
                (0,0,1,1),
                (0,0,0,0)
        );--  block_Z_1
        
        block_Z_1_pointer:aliased block_array:=block_Z_1;
        
        block_Z_2:block_array:=(
                (0,0,0,1),
                (0,0,1,1),
                (0,0,1,0),
                (0,0,0,0)
        );--  block_Z_2
        
        block_Z_2_pointer:aliased block_array:=block_Z_2;
        --  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        block_L_1:block_array:=(
                (0,0,0,0),
                (0,1,1,1),
                (0,1,0,0),
                (0,0,0,0)
        );
        
        block_L_1_pointer:aliased block_array:=block_L_1;
        
        block_L_2:block_array:=(
                (0,0,1,0),
                (0,0,1,0),
                (0,0,1,1),
                (0,0,0,0)
        );
        
        block_L_2_pointer:aliased block_array:=block_L_2;
        
        block_L_3:block_array:=(
                (0,0,0,1),
                (0,1,1,1),
                (0,0,0,0),
                (0,0,0,0)
        );
        
        block_L_3_pointer:aliased block_array:=block_L_3;
        
        block_L_4:block_array:=(
                (0,1,1,0),
                (0,0,1,0),
                (0,0,1,0),
                (0,0,0,0)
        );
        
        block_L_4_pointer:aliased block_array:=block_L_4;

        --  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        block_J_1:block_array:=(
                (0,0,0,0),
                (0,1,1,1),
                (0,0,0,1),
                (0,0,0,0)
        );
        
        block_J_1_pointer:aliased block_array:=block_J_1;
        
        block_J_2:block_array:=(
                (0,0,1,1),
                (0,0,1,0),
                (0,0,1,0),
                (0,0,0,0)
        );
        
        block_J_2_pointer:aliased block_array:=block_J_2;
        
        block_J_3:block_array:=(
                (0,1,0,0),
                (0,1,1,1),
                (0,0,0,0),
                (0,0,0,0)
        );
        
        block_J_3_pointer:aliased block_array:=block_J_3;
        
        block_J_4:block_array:=(
                (0,0,1,0),
                (0,0,1,0),
                (0,1,1,0),
                (0,0,0,0)
        );
        
        block_J_4_pointer:aliased block_array:=block_J_4;
        --  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        block_T_1:block_array:=(
                (0,0,0,0),
                (0,1,1,1),
                (0,0,1,0),
                (0,0,0,0)
        );
        
        block_T_1_pointer:aliased block_array:=block_T_1;
        
        block_T_2:block_array:=(
                (0,0,1,0),
                (0,0,1,1),
                (0,0,1,0),
                (0,0,0,0)
        );
        
        block_T_2_pointer:aliased block_array:=block_T_2;
        
        block_T_3:block_array:=(
                (0,0,1,0),
                (0,1,1,1),
                (0,0,0,0),
                (0,0,0,0)
        );
        
        block_T_3_pointer:aliased block_array:=block_T_3;
        
        block_T_4:block_array:=(
                (0,0,1,0),
                (0,1,1,0),
                (0,0,1,0),
                (0,0,0,0)
        );
        
        block_T_4_pointer:aliased block_array:=block_T_4;
        --  //////////////////////////////////////////////////////  --
        --  all possible orientations (4 for each block type) are stored 
        --  in this one-dimension array of pointers
        blocks_pointers:blocks_array:=(
        
        block_O_1_pointer'Access,block_O_1_pointer'Access,block_O_1_pointer'Access,block_O_1_pointer'Access,
        block_I_1_pointer'Access,block_I_2_pointer'Access,block_I_1_pointer'Access,block_I_2_pointer'Access,
        block_S_1_pointer'Access,block_S_2_pointer'Access,block_S_1_pointer'Access,block_S_2_pointer'Access,
        block_Z_1_pointer'Access,block_Z_2_pointer'Access,block_Z_1_pointer'Access,block_Z_2_pointer'Access,
        block_L_1_pointer'Access,block_L_2_pointer'Access,block_L_3_pointer'Access,block_L_4_pointer'Access,
        block_J_1_pointer'Access,block_J_2_pointer'Access,block_J_3_pointer'Access,block_J_4_pointer'Access,
        block_T_1_pointer'Access,block_T_2_pointer'Access,block_T_3_pointer'Access,block_T_4_pointer'Access
        
        );--  blocks
        

end blocks;