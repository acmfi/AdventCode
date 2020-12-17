#include <iostream>
#include <vector>
#include <tuple>
#include <fstream>
#include <string>

struct Instruction {
	enum InstructionType
	{
		ACC,JMP,NOP
	} instruccion;
	int parametro;
	bool ejecutada;
};


int emularHastaBucle(std::vector<Instruction>& memoria) {
	int REG_ACC = 0;
	int REG_PC = 0;

	while (!memoria[REG_PC].ejecutada) {
		switch (memoria[REG_PC].instruccion)
		{
		case Instruction::ACC:
			REG_ACC += memoria[REG_PC].parametro;
			memoria[REG_PC].ejecutada = true;
			REG_PC++;
			break;
		case Instruction::JMP:
			memoria[REG_PC].ejecutada = true;
			REG_PC += memoria[REG_PC].parametro;
			break;
		default:
			memoria[REG_PC].ejecutada = true;
			REG_PC++;
			break;
		}
	}
	return REG_ACC;
}

bool probar(std::vector<Instruction> memoria) {
	int REG_ACC = 0;
	int REG_PC = 0;

	int PREVIOUS_REG_PC = 0;
	while (REG_PC < memoria.size()) {
		if (memoria[REG_PC].ejecutada) {
			return false;
		}

		switch (memoria[REG_PC].instruccion)
		{
		case Instruction::ACC:
			REG_ACC += memoria[REG_PC].parametro;
			memoria[REG_PC].ejecutada = true;
			REG_PC++;
			break;
		case Instruction::JMP:
			memoria[REG_PC].ejecutada = true;
			PREVIOUS_REG_PC = REG_PC;
			REG_PC += memoria[REG_PC].parametro;
			break;
		default:
			memoria[REG_PC].ejecutada = true;
			PREVIOUS_REG_PC = REG_PC;
			REG_PC++;
			break;
		}
	}
	return true;
}

int fuerzaBruta(std::vector<Instruction> memoria) {
	for (auto& i : memoria) {
		if (i.instruccion == Instruction::JMP) {
			i.instruccion = Instruction::NOP;
			if(probar(memoria)) break;
			i.instruccion = Instruction::JMP;
		}
		if (i.instruccion == Instruction::NOP) {
			i.instruccion = Instruction::JMP;
			if (probar(memoria)) break;
			i.instruccion = Instruction::NOP;
		}
	}





	int REG_ACC = 0;
	int REG_PC = 0;

	int PREVIOUS_REG_PC = 0;
	while (REG_PC < memoria.size()) {

		switch (memoria[REG_PC].instruccion)
		{
		case Instruction::ACC:
			REG_ACC += memoria[REG_PC].parametro;
			memoria[REG_PC].ejecutada = true;
			REG_PC++;
			break;
		case Instruction::JMP:
			memoria[REG_PC].ejecutada = true;
			PREVIOUS_REG_PC = REG_PC;
			REG_PC += memoria[REG_PC].parametro;
			break;
		default:
			memoria[REG_PC].ejecutada = true;
			PREVIOUS_REG_PC = REG_PC;
			REG_PC++;
			break;
		}
	}
	return REG_ACC;
}

int main() {
	std::ifstream file("input.txt");

	std::vector<Instruction> memoria;


	Instruction i;
	std::string aux;
	while (file >> aux >> i.parametro) {
		if (aux == "acc") {
			i.instruccion = i.ACC;
		}
		else if (aux == "jmp") {
			i.instruccion = i.JMP;
		}
		else {
			i.instruccion = i.NOP;
		}
		i.ejecutada = false;
		memoria.push_back(i);
	}
	
	std::cout << "ACC: " << fuerzaBruta(memoria);
	return getchar();
}