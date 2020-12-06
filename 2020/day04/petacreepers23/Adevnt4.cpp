#include <iostream>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <regex>
#include <string>

struct Passport{
std::string  byr;//(Birth Year)
std::string  iyr;//(Issue Year)
std::string  eyr;//(Expiration Year)
std::string  hgt;//(Height)
std::string  hcl;//(Hair Color)
std::string  ecl;//(Eye Color)
std::string  pid;//(Passport ID)
std::string  cid;//(Country ID) //optional
bool valid;
};


void applyRegex(std::regex& regex, std::string& token, std::smatch& res, Passport& p) {
	std::regex_search(token, res, regex);
	if (res.size() == 2) {
		
	} else {
		p.valid = false;
	}
}

void parsePassport(std::vector<Passport>& vec, std::string token) {
	Passport p;
	p.valid = true;
	std::regex BYR("byr:(\\d+)");
	std::regex IYR("iyr:(\\d+)");
	std::regex EYR("eyr:(\\d+)");
	std::regex HGT("hgt:(\\d+[ci][nm])");
	std::regex HCL("hcl:#([a-fA-F0-9]{6})");
	std::regex ECL("ecl:(amb|blu|brn|gry|grn|hzl|oth)");
	std::regex PID("pid:(\\d+)");

	std::smatch res;
	res = std::smatch();
	applyRegex(BYR,token,res,p);
	p.byr = res[1];
	//std::cout << "BYR: " << p.valid <<std::endl; 

	res = std::smatch();
	applyRegex(IYR, token, res, p);
	p.iyr = res[1];
	//std::cout << "IYR: " << p.valid << std::endl;

	res = std::smatch();
	applyRegex(EYR, token, res, p);
	p.eyr = res[1];
	//std::cout << "EYR: " << p.valid << std::endl;

	res = std::smatch();
	applyRegex(HGT, token, res, p);
	p.hgt = res[1];
	//std::cout << "HGT: " << p.valid << std::endl;

	res = std::smatch();
	applyRegex(HCL, token, res, p);
	p.hcl = res[1];
	//std::cout << "HCL: " << p.valid << std::endl;

	res = std::smatch();
	applyRegex(ECL, token, res, p);
	p.ecl = res[1];
	//std::cout << "ECL: " << p.valid << std::endl;

	res = std::smatch();
	applyRegex(PID, token, res, p);
	p.pid = res[1];
	//std::cout << "PID: " << p.valid << std::endl;

	//res = std::smatch();
	//applyRegex(CID, token, res, p);
	vec.push_back(p);
}

void parsePassportString(std::vector<Passport>& vec, std::string token) {
	Passport p;
	p.valid = true;

	std::size_t found = token.find("byr");
	if (found == std::string::npos) {
		std::cout << "byr no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "BYR: " << p.valid << std::endl;

	found = token.find("iyr");
	if (found == std::string::npos) {
		std::cout << "iyr no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "IYR: " << p.valid << std::endl;

	found = token.find("eyr");
	if (found == std::string::npos) {
		std::cout << "eyr no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "EYR: " << p.valid << std::endl;

	found = token.find("hgt");
	if (found == std::string::npos) {
		std::cout << "hgt no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "HGT: " << p.valid << std::endl;

	found = token.find("hcl");
	if (found == std::string::npos) {
		std::cout << "hcl no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "HCL: " << p.valid << std::endl;

	found = token.find("ecl");
	if (found == std::string::npos) {
		std::cout << "ecl no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "ECL: " << p.valid << std::endl;

	found = token.find("pid");
	if (found == std::string::npos) {
		std::cout << "pid no encontrado" << std::endl;
		p.valid = false;
		vec.push_back(p);
		return;
	}
	std::cout << "PID: " << p.valid << std::endl;

	vec.push_back(p);
}

void parseFile(std::vector<Passport>& vec) {
	std::ifstream f("input.txt");
	std::stringstream buffer;
	buffer << f.rdbuf();
	std::string s = buffer.str();
	std::string delimiter = "\n\n";
	size_t pos = 0;
	std::string token;//ESTO TIENE ALGUN BUG, TODO:APENDEAR UN \N AL FINAL
	while ((pos = s.find(delimiter)) != std::string::npos) {
		token = s.substr(0, pos);
		//std::cout << token << std::endl;
		s.erase(0, pos + delimiter.length());
		parsePassport(vec,token);
		//std::cout << ":::::::::::::::" << vec[vec.size()-1].valid << std::endl << std::endl;
	}
}

int main() {
	std::vector<Passport> pasaportes;//286
	parseFile(pasaportes);
	size_t totales = 0;
	for (auto p : pasaportes) {
		if(p.valid) {
			try {//fuera cosas raras
				std::stoi(p.byr);
				std::stoi(p.iyr);
				std::stoi(p.eyr);
			} catch (...) {
				continue;
			}

			if(std::stoi(p.byr)<1920 || std::stoi(p.byr)>2002){
				continue;
			}
			if (std::stoi(p.iyr)<2010 || std::stoi(p.iyr)>2020) {
				continue;
			}
			if (std::stoi(p.eyr)<2020 || std::stoi(p.eyr)>2030) {
				continue;
			}
			size_t poscm = 0;
			poscm = p.hgt.find("cm");
			if (poscm != std::string::npos) {
				int altura = std::stoi(p.hgt.substr(0,poscm));
				if (altura < 150 || altura > 193) {
					continue;
				}
			} else {
				poscm = p.hgt.find("in");
				int altura = std::stoi(p.hgt.substr(0, poscm));
				if (altura < 59 || altura > 76) {
					continue;
				}
			}
			if (p.hcl.size() != 6) {
				continue;
			}
			if (p.pid.size() != 9) {
				continue;
			}
			totales++;
		}
	}
	std::cout << "TOTALES: " << totales;
	return getchar();//254
}










