#include <string>
#include <vector>

namespace P23 {
	
	/*
		Divide str en un vector de strings, delimitador por el string separator, y como maximo en maxplit secciones.

		Ejemplo: str = "Hola mundo bla bla bla"
		split(str," ") -> {"Hola", "mundo", "bla", "bla", "bla"}
		split(str,"bla") -> {"Hola mundo ", " ", " ", ""}
		split(str," ", 3) -> {"Hola", "mundo", "bla bla bla"}
	*/
	std::vector<std::string> split(const std::string& str, const std::string& separator,const size_t& maxplit = std::string::npos){
		std::vector<std::string> ret;
		if(maxplit != std::string::npos) ret.reserve(maxplit);

		size_t curOffset = 0;
		size_t curPos = 0;
		while (ret.size() < maxplit-1 && (curPos=str.find(separator, curOffset)) != std::string::npos) {
			ret.push_back(str.substr(curOffset, curPos - curOffset));
			curOffset = curPos + separator.size();
		}
		ret.push_back(str.substr(curOffset));
		return ret;
	}

}