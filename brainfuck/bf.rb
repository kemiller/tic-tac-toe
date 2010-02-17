
# By Stephen Sykes
# http://www.stephensykes.com/bf.html

class Bf
def initialize
  @a=[0]*30000
  @b=[]
  @p=0
  @x=0
end

def r
  @p+=1 if !@j
end

def l
  @p-=1 if !@j
end

def i
  @a[@p]+=1 if !@j
end

def d
  @a[@p]-=1 if !@j
end

def o
  print @a[@p].chr if !@j
end

def n
  @a[@p] = $stdin.getc if !@j
end

def j
  @x+=1
  if @a[@p]==0
    @j=true
  else
    @b[@x]=$i-1
  end
end

def e
  if @j
    @j=false
  else
    $i=@b[@x]
  end
  @x-=1
end

def b
  10.times {|a| print @a[a].chr}
end
end

b=Bf.new
$i=0
p=$<.readlines.join.tr('^><+\-.,[]#','').tr('><+\-.,[]#', 'rlidonjeb')
while $i<p.size
  b.send(p[$i].chr)
  $i+=1
end
