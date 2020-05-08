Tree's merging procedure
===

Variant 1, when donorIndex <= (recipientIndex - recipientSize)
---

0: Initial tree layout (spaced for readability):

    nml kjb ih gfedcb a
    002 011 01 010202 4
        ---    ------
        
1: add a donor's children count to the recipient children count


    nml kjb ih gfedcb a
    002 011 01 010203 4
                    ^ 

2: decrement donor's parent children count


    nml kjb ih gfedcb a
    002 011 01 010203 3
                      ^
         
3: relocate a donor tree next to the recipient tree, if needed


    nml ih kjb gfedcb a
    002 01 011 010202 4
           --- ------ 

4: remove the donor's top node


    nml ih kjgfedcb a
    002 01 01010203 4
           --