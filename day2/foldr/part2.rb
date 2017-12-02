a=[]
puts loop{if l=gets then a<<l else break a end}
       .map{|l|l.chomp.split(' ')
              .map(&:to_i)
              .combination(2)
              .select{|p|p[0]%p[1]==0 or p[1]%p[0]==0}
              .map{|p| if p[0]%p[1]==0 then p else p.reverse end}
              .flatten.reduce(&:/)}.sum
