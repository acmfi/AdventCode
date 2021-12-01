namespace Day_1
{
    class Program
    {

        static int Sonar_Sweep(string fichero)
        {
            int i = 0;
            int answer = 0;

            int lineCounter = 0;
            using (var reader = File.OpenText(fichero)) {
                while (reader.ReadLine() != null)
                {
                    lineCounter = lineCounter + 1;
                }
            }

            while (i <= lineCounter-2) {
                int lineA = Convert.ToInt32(File.ReadLines(fichero).Skip(i).Take(1).First());
                int lineB = Convert.ToInt32(File.ReadLines(fichero).Skip(i+1).Take(1).First());
                if (lineB > lineA) 
                {
                    answer = answer + 1;
                }

                i = i + 1;
            }

            return answer;
        }

        static void Main(string[] args)
        {
            string file;
            Console.WriteLine("Please write the input file path");
            file = Console.ReadLine();
            System.Console.WriteLine(Sonar_Sweep(file));
        }
    }
}