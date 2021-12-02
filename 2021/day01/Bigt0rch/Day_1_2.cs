namespace Day_1
{
    class Program
    {

        static int Sonar_Sweep(string fichero)
        {
            int i = 0;
            int answer = 0;

            int lineCounter = 0;
            using (var reader = File.OpenText(fichero))
            {
                while (reader.ReadLine() != null)
                {
                    lineCounter = lineCounter + 1;
                }
            }

            while (i <= lineCounter - 4)
            {
                int lineA = Convert.ToInt32(File.ReadLines(fichero).Skip(i).Take(1).First());
                int lineB = Convert.ToInt32(File.ReadLines(fichero).Skip(i + 1).Take(1).First());
                int lineC = Convert.ToInt32(File.ReadLines(fichero).Skip(i + 2).Take(1).First());
                int lineD = Convert.ToInt32(File.ReadLines(fichero).Skip(i + 3).Take(1).First());

                int group1 = lineA + lineB + lineC;
                int group2 = lineB + lineC + lineD;
                if (group2 > group1)
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
            Console.WriteLine("Please write the input (.txt) file path");
            file = Console.ReadLine();
            System.Console.WriteLine("The number of times your distance increases is " + Sonar_Sweep(file));
        }
    }
}