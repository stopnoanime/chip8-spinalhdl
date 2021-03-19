#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <iomanip>

int main( int argc, char *argv[] )
{
    if (argc < 2){
        std::cout<<"No files specified"<<std::endl;
        return -1;
    }

    if (argc > 257){
        std::cout<<"Too many files specified. CHIP-8 can fit a maximum of 256 files."<<std::endl;
        return -1;
    }

    std::ifstream input_file;
    std::ofstream output_file;
    std::vector<uint8_t> buffer;
    uint8_t tmp;

    output_file.open("hex_out.txt", std::ios::out);

    for(int i = 0; i < argc - 1; i++){
        input_file.open(argv[i+1], std::ios::in | std::ios::binary);

        if(!input_file){
            std::cout<<"Couldn't open file "<<argv[i+1]<<std::endl;
            return -1;
        }

        while ( input_file.read(reinterpret_cast<char*>(&tmp), sizeof(uint8_t)) ) buffer.push_back(tmp);

        if (buffer.size() > 4096 - 512) {
            std::cout<<"File "<<argv[i+1]<<" is to larrge to fit on CHIP-8 RAM."<<std::endl;
            return -1;
        }

        while (buffer.size() < 4096 - 512) buffer.push_back(0);
        
        for ( unsigned int i = 0; i < buffer.size(); i++ ) {
            output_file<<std::setfill('0') <<std::hex <<std::setw(2) <<static_cast<int>(buffer[i]);
        };

        buffer.clear();
        input_file.close();
    }

    output_file.close();

    std::cout<<"Sucesfully combined all files."<<std::endl;

    return 0;
}