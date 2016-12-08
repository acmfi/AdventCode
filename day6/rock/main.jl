import DataStructures.counter

inp = Array{Char}(8, 2)

f = open("input.txt")
for ln in eachline(f)
    ar = collect(strip(ln))
    inp = hcat(ar, inp)
end

for j = 1:size(inp, 1)

    winner = '\0'::Char
    winn = 0

    cs = counter(inp[j,:])

    for k in keys(cs)
        if k != '\0' && cs[k] > winn
            winner = k
            winn = cs[k]
        end
    end

    print(winner)
end

print("\n")

# Answer1: tzstqsua
