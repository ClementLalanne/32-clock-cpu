# -*- coding: utf-8 -*-
import pdb
pdb.pm

def dec2bin(d,nb=16):
    """Représentation d'un nombre entier en chaine binaire (nb: nombre de bits du mot)"""
    if d == 0:
        return "0".zfill(nb)
    if d<0:
        d += 1<<nb
    b=""
    while d != 0:
        d, r = divmod(d, 2)
        b = "01"[r] + b
    return b.zfill(nb)

def compile_byte(filename_input=str, filename_output=str):
    i = open(filename_input, "r")
    o = open(filename_output, "w")
    mips = {'ADD' : ['000000', '00000100000'],
            'ADDI' : '001000',
            'BEQ' : '000100',
            'BGEZ' : ['000001', '00001'],
            'J' : '000010',
            'JAL' : '000011',
            'LW' : '100011', 
            'SUB': ['000000', '00000100010'],
            'SW': '101011'
            }
            
    reg = {'R0' : '00000',
            '$clk' : '11110',
            '$sec' : '00011',
            '$min' : '00100',
            '$hrs' : '00101',
            '$day' : '00110',
            '$wek' : '00111',
            '$mon' : '01000',
            '$yea' : '01001',
            '$bis' : '01010',
            '$cen' : '01011',
            '$fou' : '01100',
            '$sxt' : '01101',
            '$tfr' : '01110',
            '$sev' : '01111',
            '$lmo' : '10000',
            '$twl' : '10001',
            '$4' : '10010',
            '$100' : '10011',
            '$400' : '10100',
            '$60' : '10101',
            '$24' : '10110',
            '$7' : '10111',
            '$12' : '11000',
            '$x' : '11010'}
    mem = {'$zer':'0000000010000000',
'$mmm':'0000000010000100',
'$in1':'0000000010001000',
'$in2':'0000000010001100',
'$in3':'0000000010010000',
'$in4':'0000000010010100',
'$in5':'0000000010011000',
'$in6':'0000000010011100',
'$in7':'0000000010100000',
'$in8':'0000000010100100',
'$in9':'0000000010101000',
'$in0':'0000000010101100',
    }
    
    
    cpt = 0
    for ligne in i:     # Extract data of a line separated by spaces
        cpt += 1
        l = ligne.rstrip('\n\r')    #remove carriage return, return line
        l = l.strip()               #remove extra spaces
        l = l.split(' ')            #split the line in words between line
        instr = l[0]
        if instr == 'ADD':            
            o.write(mips[instr][0])
            #adress            
            o.write(reg[l[2]])
            o.write(reg[l[3]])
            o.write(reg[l[1]])
            #
            o.write(mips[instr][1])
            
        elif instr == 'ADDI':
            o.write(mips[instr])
            #adress
            o.write(reg[l[2]])
            o.write(reg[l[1]])
            o.write(dec2bin(int(l[3])))
            
        elif instr == 'BEQ' :
            o.write(mips[instr])
            #adress
            o.write(reg[l[1]])
            o.write(reg[l[2]])
            o.write(dec2bin(int(l[3])))
            
        elif instr == 'BGEZ' :
            o.write(mips[instr][0])
            #adress
            o.write(reg[l[1]])
            o.write(mips[instr][1])
            o.write(dec2bin(int(l[2])))
            
        elif instr == 'J' :
            o.write(mips[instr])
            #adress
            o.write(dec2bin(int(l[1]),26))         
            
        elif instr == 'JAL' :
            o.write(mips[instr])
            #adress
            o.write(dec2bin(int(l[1]),26))         
            
        elif instr == 'LW' : 
            o.write(mips[instr])
            #adress
            o.write(reg[l[1]])
            o.write(reg[l[2]])
            o.write(mem[l[3]])            
            
        elif instr == 'SUB' : 
            o.write(mips[instr][0])
            #adress
            o.write(reg[l[2]])
            o.write(reg[l[3]])
            o.write(reg[l[1]])
            o.write(mips[instr][1])
 
        elif instr == 'SW' : 
            o.write(mips[instr])
            #adress
            o.write(reg[l[1]])
            o.write(reg[l[2]])
            o.write(mem[l[3]])
        else:
            print "Error instruction in asm file line : " + str(cpt)
        
        if not (l == [''] or l[0] == '#'):
            o.write('\n')
        
    i.close()
    o.close()

compile_byte("clock.ass", "clock.byte")

