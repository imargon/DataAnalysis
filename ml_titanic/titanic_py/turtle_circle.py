# -*- encoding:utf-8 _*_

from termcolor import colored #颜色输出模块
import random
import time
from turtle import Turtle


def draw_circle():
    t = Turtle()
    t.setpos(-200, 0)
    for i in range(60):
        t.forward(400)
        t.color('red')
        t.right(170)
        time.sleep(3)
        t.done()


def rand_rgb():
    return (random.randint(0,255), random.randint(0, 255), random.randint(0, 255))

def rand_hsl():
    h = random.uniform(0.02, 0.31) + random.choice([0, 1/3.0, 2/3.0])    
    l = random.uniform(0.3, 0.8)
    s = random.uniform(0.3, 0.8)
    
    rgb = colorsys.hls_to_rgb(h,  l,  s)
    return (int(rgb[0]*256), int(rgb[1]*256), int(rgb[2]*256))
    
    
def run():
        print 'Hi'
        print colored('hello', 'red'), colored('world', 'green')
        
if __name__ == '__main__':
    rand_rgb()
    run()
    draw_circle()