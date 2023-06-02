class Person
    attr_accessor :name, :age
    def initialize(name, age)
        @name = name
        @age = age
    end
    def getAge
        return @age
    end
    def setAge(age)
        @age = age
        return self
    end
end

class Student < Person
    attr_accessor :grade
    def initialize(name, age, grade)
        super(name, age)
        @grade = grade
    end
    def getGrade
        return @grade
    end
    def changeGrade(grade)
        @grade = grade
        return self
    end
end
#staff
class Staff < Person
    attr_accessor :position
    def initialize(name, age, position)
        super(name, age)
        @position = position
        return self
    end
    def getPosition
        return @position
    end
    def changePosition(newPosition)
        @position = newPosition
        return self
    end
end

class Roster
    attr_accessor :HashList, :count
    def initialize
        @HashList = Hash.new
        @count = 0
    return self
    end
    def add(person)
        @HashList[person.name] = person
        @count += 1
        return nil
    end
    def size
        return @count
    end
    def remove(person)
        @HashList.each do |key, value|
            if person.name == value.name
                @HashList.delete(key)
                @count -= 1
            end
        end
    end
    def getPerson(name)
        @HashList.each do |key, value|
            if name == key
                return value
            end
        end
        return nil
    end
    def map
        if block_given?
            @HashList.each do |key, value|
                if value.class == Person
                    yield value
                end
            end
        end
    end
end
