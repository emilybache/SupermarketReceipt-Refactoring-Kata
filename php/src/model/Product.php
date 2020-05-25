<?php

declare(strict_types=1);

namespace Supermarket\model;

class Product
{
    /**
     * @var string
     */
    private $name;

    /**
     * @var string
     */
    private $unit;

    public function __construct(string $name, string $unit)
    {
        $this->name = $name;
        $this->unit = $unit;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getUnit(): string
    {
        return $this->unit;
    }
}
